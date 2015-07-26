#' Aggregate many topic models in to an ensemble
#' @description This function aggregates output of several topic models into a single ensemble topic model.
#' 
#' @param posteriors = list of models. Each element contains two
#'       matrices, "theta" and "phi". "theta"
#'       is the posterior prediction of topics distributed over
#'       documents. "phi" is the posterior prediction of 
#'       terms distributed over topics.
#' @param assignments = a vector where each element corresponds to a 
#'       single topic from a single model. So, for example, if you 
#'       have 100 models of 50 topics, this vector will be of length
#'       100 * 50 = 5000. The entries of the vector are numeric, 
#'       characters, or factors and provide a mapping of topics between 
#'       models. The names of this vector, correspond to *unique* names
#'       of topics in each model.
#' @param weights = vector of weights of each model in aggregating. If this
#'       is NULL, the default, each model is given equal weighting in 
#'       aggregation.
#' @param drops = vector of values of assignments indicating which assignments 
#'       of topics we want to drop. If NULL, the default, no topics are
#'       dropped. 
#' @details
#'   Output:
#'       a list with two elements, "theta" and "phi" of
#'       the ensemble model.
#' @export
#' @examples
#' lex <- ConvertDtm2Docs(dtm)
#' lex <- lexicalize(lex, vocab=colnames(dtm))
#' my_posteriors <- lapply(1:100, function(iteration){
#'   lda <- lda.collapsed.gibbs.sampler(documents = lex, K = 100, vocab = colnames(dtm), num.iterations=2000, alpha=0.1, eta=0.05)
#'   lda <- FormatRawLdaOutput(lda.result=lda, docnames=rownames(dtm), smooth=TRUE)
#' })
#' 
#' for(j in 1:length(myposteriors)){
#'   rownames(myposteriors[[ j ]]$theta) <- paste(rownames(myposteriors[[ j ]]$theta), j, sep="_")
#'   colnames(myposteriors[[ j ]]$phi) <- paste(colnames(myposteriors[[ j ]]$phi), j, sep="_")
#' }
#' 
#' myD <- EnsembleTopicDist(posteriors=myposteriors, method="cosine")
#' 
#' myH <- hclust(myD, method="ward.D")
#' 
#' myassignments <- cutree(myH, 100)
#' 
#' ensemble_lda <- AggregateTopicModels(posteriors=myposteriors, assignments=myassignments)

AggregateTopicModels <- function(posteriors, assignments, weights=NULL, drops=NULL){
  ##########################################################################
  
  # pull out phi and theta from each model
  # this is done to avoid a memory leak later on
  theta <- lapply(posteriors, function(x) x$theta)
  
  phi <- lapply(posteriors, function(x) x$phi)
  
  # Rename topics in each model according to assignments
  # remove any topics/assignments in "drops"
  # this will silently drop these as we move forward
  if( ! is.null(drops) ) assignments <- assignments[ ! assignments %in% drops ]
  
  nameFun <- function(x){ # assumes theta, must transpose for phi
    # get column/row names in order
    # this first step will FAIL if your names aren't in order before starting
    names.assigned <- assignments[ names(assignments) %in% colnames(x) ]
    
    x <- x[ , names(names.assigned) ] # make sure columns are in correct order
    colnames(x) <- names.assigned
    
    ########
    # if you're going to collapse topics in the same cluster w/i a model, do it here.
    ########
    
    return(x)
  }
  
  theta <- lapply(theta, nameFun)
  
  phi <- lapply(phi, function(x){
    result <- nameFun( x=t(x) )
    
    return(t(result))
  })
  
  # Apply weights
  if( is.null(weights) ) weights <- rep(1/length(posteriors), length(posteriors) ) # if you don't have your own weights...
  
  weightFun <- function(weight, posterior){
    result <- weight * posterior
    return(result)
  }
  
  theta <- mapply(weightFun, weight=weights, posterior=theta, SIMPLIFY = FALSE)
  
  phi <- mapply(weightFun, weight=weights, posterior=phi, SIMPLIFY = FALSE)
  
  # Combine matrices together in one swoop
  theta <- do.call(cbind, theta)
  
  phi <- do.call(rbind, phi)
  
  # Combine (sum) columns/rows with the same assignments
  
  theta <- theta[ , sort(colnames(theta) ) ]
  
  phi <- phi[ sort(rownames(phi)) , ]    
  
  agg.topics <- sort(unique(colnames(theta)))
  
  indices <- lapply(agg.topics, function(x)  which(colnames(theta) == x )) # grep(x, colnames(theta))
  
  names(indices) <- paste("t.", agg.topics, sep="")
  
  theta <- sapply(indices, function(x){
    result <- Matrix::rowSums(theta[ , x ])
    return(result)
  })
  
  phi <- sapply(indices, function(x){
    result <- Matrix::colSums(phi[ x , ])
    return(result)
  })
  
  # normalize rows/columns as appropriate
  theta <- theta / Matrix::rowSums(theta)    
  
  phi <- t(phi)
  
  phi <- phi / Matrix::rowSums(phi)
  
  
  # finally, return the result
  return(list(theta=theta, phi=phi))
  
}
