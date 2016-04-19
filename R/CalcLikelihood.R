#' @name CalcLikelihood
#' @title Calculate the log likelihood of a document term matrix given a topic model
#' @param dtm The document term matrix of class \code{dgCMatrix}.
#' @param phi The phi matrix whose rows index topics and columns index words. 
#' The i, j entries are P(word_i | topic_j)  
#' @param theta The theta matrix whose rows index documents and columns index topics. 
#' The i, j entries are P(topic_i | document_j)
#' @param ... Other arguments to pass to \code{\link[textmineR]{TmParallelApply}}. See note, below. 
#' @description
#'     This function takes a DTM, phi matrix (P(word|topic)), and a theta matrix 
#'     (P(topic|document)) and returns a single value for the likelihood of the 
#'     data given the model.     
#' @return
#' Returns an object of class \code{numeric} corresponding to the log likelihood.
#' @note
#' This function performs parallel computation if \code{dtm} has more than 3,000
#' rows. The default is to use all available cores according to \code{\link[parallel]{detectCores}}.
#' However, this can be modified by passing the \code{cpus} argument when calling
#' this function.
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm) 
#' data(nih_sample_topic_model)
#' 
#' # Get the likelihood of the data given the fitted model parameters
#' ll <- CalcLikelihood(dtm = nih_sample_dtm, 
#'                      phi = nih_sample_topic_model$phi, 
#'                      theta = nih_sample_topic_model$theta)
#' 
#' ll
#' @export
CalcLikelihood <- function(dtm, phi, theta, ...){
  
  # check that inputs have necessary formats
  if(nrow(dtm) != nrow(theta) ){ 
    # number of documents matches?
    stop("nrow(dtm) must match nrow(theta).")
  }
  
  if(nrow(phi) != ncol(theta)){ 
    # number of topics matches?
    stop("nrow(phi) must match ncol(theta)")
  }
  
  if(ncol(dtm) != ncol(phi)){ 
    # vocabulary size matches?
    stop("ncol(dtm) must match ncol(phi)")
  }
  
  if(is.null(rownames(dtm)) | is.null(rownames(theta))){ 
    # doc names exist?
    warning("missing rownames from one or both of dtm and theta. Row index used instead.")
    rownames(dtm) <- 1:nrow(dtm)
    rownames(theta) <- 1:nrow(theta)
  }
  
  if(is.null(colnames(dtm)) | is.null(colnames(phi))){ 
    # term names exist?
    warning("missing colnames from one or both of dtm and phi. Column index used instead")
    colnames(dtm) <- 1:ncol(dtm)
    colnames(phi) <- 1:ncol(phi)
  }
  
  if(is.null(rownames(phi)) | is.null(colnames(theta))){ 
    # topic names exist?
    warning("missing colnames from theta or rownames from phi. Row/column indeces used instead.")
    colnames(theta) <- 1:ncol(theta)
    rownames(phi) <- 1:nrow(phi)
  }
  
  if(sum(intersect(colnames(dtm), colnames(phi)) %in% union(colnames(dtm), colnames(phi))) != ncol(dtm)){
    # all terms in dtm present in phi?
    stop("vocabulary does not match between dtm and phi. Check colnames of both matrices.")
  }
  
  if(sum(intersect(rownames(dtm), rownames(theta)) %in% union(rownames(dtm), rownames(theta))) != nrow(dtm)){
    # all documents in dtm present in theta?
    stop("document names do not match between dtm and theta. Check rownames of both matrices.")
  }
  
  if(sum(intersect(colnames(theta), rownames(phi)) %in% union(colnames(theta), rownames(phi))) != ncol(theta)){
    # all topics in theta present in phi?
    stop("topic names do not match. Check rownames(phi) and colnames(theta)")
  }

  # ensure that all inputs are sorted correctly
  phi <- phi[ colnames(theta) , colnames(dtm) ]
  
  theta <- theta[ rownames(dtm) , ]
  
  # do in parallel in batches of about 3000 if we have more than 3000 docs
  if(nrow(dtm) > 3000){
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    data_divided <- lapply(batches, function(j){
      
      dtm_divided <- dtm[ j:min(j + 2999, nrow(dtm)) , ]
      
      theta_divided <- theta[ j:min(j + 2999, nrow(dtm)) , ]
      
      list(dtm_divided=dtm_divided, theta_divided=theta_divided)
    })
    
    result <-TmParallelApply(X = data_divided, FUN = function(x){
      CalcLikelihoodC(dtm=x$dtm_divided, 
                      phi=phi, 
                      theta=x$theta_divided)
    }, export=c("phi"), ...)
    
    result <- sum(unlist(result))
    
    # do sequentially otherwise
  }else{
    result <- CalcLikelihoodC(dtm=dtm, phi=phi, theta=theta)
  }
  
  result  
}
