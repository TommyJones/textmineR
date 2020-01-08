#' Update methods for topic models
#' @description \code{update} will update a previously-trained topic model based
#' on new data. Useful for updates or transfer learning.
#' @param object An existing trained topic model
#' @param ... Additional arguments to the call
#' @export
update <- function(object, ...) UseMethod("update")

#' Posterior methods for topic models
#' @description \code{posterior} will draw from the posterior distribution of a
#' topic model
#' @param object An existing trained topic model
#' @param ... Additional arguments to the call
#' @export
posterior <- function(object, ...) UseMethod("posterior")


#' Draw from the posterior of an LDA topic model
#' @description This function takes an object of class \code{lda_topic_model} and
#' draws samples from the posterior of either \code{phi} or \code{theta}. This is 
#' useful for quantifying uncertainty around parametrs of the final model.
#' @param object An object of class \code{lda_topic_model}
#' @param which A character of either 'theta' or 'phi', indicating from which
#' matrix to draw posterior samples
#' @param num_samples Integer number of samples to draw
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}.
#' @references 
#' Heinrich, G. (2005) Parameter estimation for text analysis. Technical report. 
#' \href{http://www.arbylon.net/publications/text-est.pdf}{http://www.arbylon.net/publications/text-est.pdf}
#' @return Returns a data frame where each row is a single sample from the posterior. 
#' Each column is the distribution over a single parameter. The variable \code{var}
#' is a facet for subsetting by document (for theta) or topic (for phi).
#' @export
#' @examples
#' \dontrun{
#' a <- posterior(object = nih_sample_topic_model, which = "theta", num_samples = 20)
#' 
#' plot(density(a$t1[a$var == "8693991"]))
#' 
#' b <- posterior(object = nih_sample_topic_model, which = "phi", num_samples = 20)
#' 
#' plot(denisty(b$research[b$var == "t_5"]))
#' }
posterior.lda_topic_model <- function(object, which = "theta", num_samples = 100, ...) {
  
  ### check inputs ----
  if (! class(object) == "lda_topic_model") 
    stop("object must be of class lda_topic_model")
  
  if (num_samples <= 0) {
    stop("num_samples must be an integer greater than 0")
  }
  
  num_samples <- round(num_samples) # in case someone is cheeky
  
  
  ### set up objects to extract ----
  if (which == "theta") {
    # extract dirichlet parameter for theta
    # par <- object$theta * (sum(object$alpha) + rowSums(object$data))
    
    par <- t(object$counts$theta_counts) + object$alpha
    
    par <- t(par)
    
    colnames(par) <- colnames(object$theta)
    
    rownames(par) <- rownames(object$theta)
    
  } else if (which == "phi") {
    
    # # need to recover approximate theta count mat to get number of times
    # # each topic was sampled
    # theta_count <- object$theta * (sum(object$alpha) + rowSums(object$data)) 
    # theta_count <- t(theta_count) - object$alpha 
    # theta_count <- t(theta_count) %>% round %>% colSums()
    # 
    # # now get the right parameter matrix
    # par <- object$phi * (sum(object$beta) + theta_count)
    
    if (class(object$beta) == "matrix") {
      
      par <- object$counts$phi_counts + object$beta
      
    } else {
      
      par <- t(object$counts$phi_counts) + object$beta
      
      par <- t(par)
      
    }
    

    
    colnames(par) <- colnames(object$phi)
    
    rownames(par) <- rownames(object$phi)
    
  } else {
    stop("which must be one of 'theta' or 'phi'")
  }
  
  # get appropriate dim names to use later
  cols <- colnames(par)
  
  rows <- rownames(par)
  
  
  ### take samples ----
  
  # sample from each row (document or topic)
  samples <- lapply(seq_len(nrow(par)), function(j) par[j,]) %>%
    textmineR::TmParallelApply(function(x){
      p <- gtools::rdirichlet(n = num_samples, alpha = x)
      
      colnames(p) <- cols
      
      as.data.frame(p)
    }, export = c("num_samples", "cols"), libraries = "gtools", ...)
  
  
  samples <- mapply(function(x,y){
    x$var <- y
    x
  },x = samples, y = rows,
  SIMPLIFY = FALSE)
  
  # names(samples) <- rows
  # 
  # class(samples) <- "lda_posterior"
  
  samples <- do.call(rbind, samples)
  
  # samples <- cbind(var = samples$var, samples[ -ncol(samples), ])
  
  samples
  
}


#' Calculate a matrix whose rows represent P(topic_i|tokens)
#' 
#' @description This function takes a phi matrix (P(token|topic)) and a theta 
#' matrix (P(topic|document)) and returns the phi prime matrix (P(topic|token)). 
#' Phi prime can be used for classifying new documents and for alternative
#' topic labels.
#' 
#' @param phi The phi matrix whose rows index topics and columns index words. 
#'            The i, j entries are P(word_i | topic_j)
#' @param theta The theta matrix whose rows index documents and columns index 
#'              topics. The i, j entries are P(topic_i | document_j)
#' @param p_docs A numeric vector of length \code{nrow(theta)} that is 
#'               proportional to the number of terms in each document. This is
#'               an optional argument. It defaults to NULL
#' @param correct Logical. Do you want to set NAs or NaNs in the final result to
#'                zero? Useful when hitting computational underflow. Defaults to
#'                \code{TRUE}. Set to \code{FALSE} for troubleshooting or diagnostics.
#' @return
#' Returns a \code{matrix} whose rows correspond to topics and whose columns
#' correspond to tokens. The i,j entry corresponds to P(topic_i|token_j)
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_topic_model) 
#' 
#' # Make a gamma matrix, P(topic|words)
#' gamma <- CalcGamma(phi = nih_sample_topic_model$phi, 
#'                    theta = nih_sample_topic_model$theta)
#' 
CalcGamma <- function(phi, theta, p_docs = NULL, correct = TRUE){
  
  # set up constants
  D <- nrow(theta)
  K <- ncol(theta)
  V <- ncol(phi)
  
  # probability of each document (assumed to be equiprobable)
  if(is.null(p_docs)){
    p_d <- rep(1/nrow(theta), nrow(theta))
  }else{
    if(sum(is.na(p_docs)) > 0){
      warning("found missing values in p_docs. Setting them as 0.")
      p_docs[ is.na(p_docs) ] <- 0 
    }
    p_d <- p_docs / sum(p_docs)
  }
  
  # get the probability of each topic
  p_t <- p_d %*% theta
  
  # get the probability of each word from the model    
  p_w <- p_t %*% phi
  
  
  
  # get our result
  gamma <- matrix(0, ncol=ncol(p_t), nrow=ncol(p_t))
  diag(gamma) <- p_t
  
  gamma <- gamma %*% phi
  
  gamma <- t(apply(gamma, 1, function(x) x / p_w))
  
  rownames(gamma) <- rownames(phi)
  colnames(gamma) <- colnames(phi)
  
  # give us zeros instead of NAs when we have NA or NaN entries
  if (correct) {
    gamma[is.na(gamma)] <- 0 
  }
  
  return(gamma)
}


#' Represent a document clustering as a topic model
#' @description Represents a document clustering as a topic model of two matrices.
#' phi: P(term | cluster) theta: P(cluster | document)
#' @param dtm A document term matrix of class \code{dgCMatrix} or whose class 
#' inherits from the \code{Matrix} package. Columns must index terms, rows must 
#' index documents.
#' @param clustering A vector of length \code{nrow(dtm)} whose entries form a
#' partitional clustering of the documents.
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}.
#' @return Returns a list with two elements, phi and theta. 'phi' is a matrix 
#' whose j-th row represents P(terms | cluster_j). 'theta' is a matrix whose
#' j-th row represents P(clusters | document_j). Each row of theta should only
#' have one non-zero element.
#' @examples
#' \dontrun{
#' # Load pre-formatted data for use
#' data(nih_sample_dtm)
#' data(nih_sample) 
#' 
#' result <- Cluster2TopicModel(dtm = nih_sample_dtm, 
#'                              clustering = nih_sample$IC_NAME)
#' }
#' @export 
Cluster2TopicModel <- function(dtm, clustering, ...){
  
  # Check inputs
  # TO DO
  
  # Set up some objects 
  iterator <- data.frame(id = rownames(dtm), 
                         clust = clustering, 
                         stringsAsFactors = FALSE)
  
  iterator <- by(iterator, INDICES = iterator$clust, function(x){
    docs <- x$id
    clust <- x$clust[ 1 ]
    list(docs = docs, clust = clust)
  })
  
  cnames <- unique(clustering)
  
  # Get theta
  theta <- TmParallelApply(X = iterator, FUN = function(x){
    m <- matrix(0, nrow = length(x$docs), ncol = length(cnames))
    rownames(m) <- x$docs
    colnames(m) <- cnames
    
    m[ , as.character(x$clust) ] <- 1
    
    m
  }, ...)
  
  theta <- do.call(rbind, theta)
  
  # if cluster names appear to be numeric, append "c" to them
  if(sum(grepl("[^0-9]", colnames(theta))) == 0 ){ 
    colnames(theta) <- paste("c", colnames(theta), sep = "_")
  }
  
  # sort rows of theta to line up with the dtm
  theta <- theta[ rownames(dtm) , ]
  
  # Get phi
  
  phi <- TmParallelApply(X = iterator, FUN = function(x){
    sub <- dtm[ x$docs , ]
    
    if( ! is.null(dim(sub))){
      Matrix::colSums(dtm[ x$docs , ]) / sum(Matrix::colSums(dtm[ x$docs , ]))
    }else{
      sub / sum(sub)
    }
    
  }, ...)
  
  phi <- do.call(rbind, phi)
  
  rownames(phi) <- colnames(theta)
  colnames(phi) <- colnames(dtm)
  
  # Get gamma
  gamma <- CalcGamma(phi, theta, p_docs = Matrix::rowSums(dtm))

  return(list(theta = theta, phi = phi, gamma = gamma, data = dtm))
}


#' @title Fit a Correlated Topic Model
#' @description A wrapper for the \link[topicmodels]{CTM} function based on 
#' Blei's original code that returns a nicely-formatted topic model.
#' @param dtm A document term matrix of class \code{dgCMatrix}
#' @param k Number of topics
#' @param return_all Logical. Do you want the raw results of the underlying 
#' function returned along with the formatted results? Defaults to \code{TRUE}.
#' @param calc_coherence Do you want to calculate probabilistic coherence of topics
#'        after the model is trained? Defaults to \code{TRUE}. 
#' @param calc_r2 Do you want to calculate R-squared after the model is trained?
#'        Defaults to \code{FALSE}.
#' @param ... Other arguments to pass to \link[topicmodels]{CTM} or \link[textmineR]{TmParallelApply}. 
#' @return Returns a list with a minimum of two objects, \code{phi} and 
#' \code{theta}. The rows of \code{phi} index topics and the columns index tokens.
#' The rows of \code{theta} index documents and the columns index topics.
#' @examples
#' # Load a pre-formatted dtm 
#' data(nih_sample_dtm) 
#' 
#' # Fit a CTM model on a sample of documents
#' model <- FitCtmModel(dtm = nih_sample_dtm[ sample(1:nrow(nih_sample_dtm) , 10) , ], 
#'                      k = 3, return_all = FALSE)
#' @export
FitCtmModel <- function(dtm, k, calc_coherence = TRUE, 
                        calc_r2 = FALSE, return_all = TRUE, ...){
  
  ## TODO - Add checks for inputs
  
  # match arguments in ... to the functions to which they apply
  dots <- list(...)
  
  parallel_args <- dots[ names(dots) %in% 
                        names(formals(textmineR::TmParallelApply)) ]
  
  model_args <- dots[ names(dots) %in% 
                      names(formals(topicmodels::CTM)) ]
  
  model <- topicmodels::CTM(x = dtm, k = k, control = model_args)
  
  theta <- model@gamma
  
  rownames(theta) <- model@documents
  colnames(theta) <- paste("t", 1:ncol(theta), sep = "_")
  
  phi <- exp(model@beta)
  colnames(phi) <- model@terms
  rownames(phi) <- colnames(theta)
  
  result <- list(theta = theta, phi = phi)
  
  result$gamma <- CalcGamma(result$phi, result$theta, 
                            p_docs = Matrix::rowSums(dtm))
  
  
  result$data <- dtm

  if (calc_coherence) {
    result$coherence <- CalcProbCoherence(result$phi, dtm)
  }
  
  if (calc_r2) {
    result$r2 <- CalcTopicModelR2(dtm, result$phi, result$theta,
                                  parallel_args)
  }
  
  if(return_all){
    result <- c(result, etc = model)
  }
  
  class(result) <- "ctm_topic_model"
  
  return(result)
}

#' Predict method for Correlated topic models (CTM)
#' @description Obtains predictions of topics for new documents from a fitted CTM model
#' @param object a fitted object of class "ctm_topic_model"
#' @param newdata a DTM or TCM of class dgCMatrix or a numeric vector
#' @param ... further arguments passed to or from other methods.
#' @return a "theta" matrix with one row per document and one column per topic
#' @note
#' Predictions for this method are performed using the "dot" method as described
#' in the textmineR vignette "c_topic_modeling".
#' @examples
#' # Load a pre-formatted dtm 
#' \dontrun{
#' data(nih_sample_dtm) 
#' 
#' model <- FitCtmModel(dtm = nih_sample_dtm[1:20,], k = 3,
#'                      calc_coherence = FALSE, calc_r2 = FALSE)
#' 
#' # Get predictions on the next 50 documents
#' pred <- predict(model, nih_sample_dtm[21:100,])
#' }
#' @export
predict.ctm_topic_model <- function(object, newdata, ...) {
  ### Check inputs ----

  if (class(object) != "ctm_topic_model") {
    stop("object must be a topic model object of class ctm_topic_model")
  }
  
  if (sum(c("dgCMatrix", "numeric") %in% class(newdata)) < 1) {
    stop("newdata must be a matrix of class dgCMatrix or a numeric vector")
  }
  
  if (class(newdata) == "numeric") { # if newdata is a numeric vector, assumed to be 1 document
    if (is.null(names(newdata))) {
      stop("it looks like newdata is a numeric vector without names. 
           Did you mean to pass a single document?
           If so, it needs a names attribute to index tokens")
    }
    
    # align vocabulary
    intersect_names <- intersect(names(newdata), colnames(object$gamma))
    
    if (length(intersect_names) == 0)
      stop("No common vocabulary terms between model and newdata. 
           See `intersect(names(newdata), colnames(object$gamma))`")
    
    newdata <- newdata[intersect_names]
    
    # coerce into a dgCMatrix
    newdata <- Matrix::Matrix(newdata, nrow = 1, sparse = TRUE)
    
    colnames(newdata) <- intersect_names
    
  } else { # we still need to align the vocabulary
    intersect_names <- intersect(colnames(newdata), colnames(object$gamma))
    
    newdata <- newdata[,intersect_names]
  }
  

  ### get predictions ----
  if (class(newdata) == "numeric") {
    newdata <- newdata / sum(newdata)
  } else {
    newdata <- newdata / Matrix::rowSums(newdata,na.rm = TRUE)
  }

  newdata[is.na(newdata)] <- 0
  
  out <- newdata %*% t(object$gamma[,intersect_names])
  
  out <- as.matrix(out)
  
  out <- out / rowSums(out) # make sure rows are normalized
  
  return(out)
}

#' Fit a topic model using Latent Semantic Analysis
#' @description A wrapper for \code{RSpectra::svds} that returns 
#' a nicely-formatted latent semantic analysis topic model.
#' @param dtm A document term matrix of class \code{Matrix::dgCMatrix}
#' @param k Number of topics
#' @param calc_coherence Do you want to calculate probabilistic coherence of topics
#'        after the model is trained? Defaults to \code{TRUE}. 
#' @param return_data Do you want to return the input DTM/TCM (given by argument
#'        \code{dtm})? Defaults to \code{FALSE}.
#' @param return_all Should all objects returned from \code{RSpectra::svds} be
#'        returned here? Defaults to \code{FALSE}
#' @param ... Other arguments to pass to \code{\link[RSpectra]{svds}} through 
#'        its \code{opts} parameter. 
#' @return Returns a list with a minimum of three objects: \code{phi},  
#' \code{theta}, and \code{sv}. The rows of \code{phi} index topics and the 
#' columns index tokens. The rows of \code{theta} index documents and the 
#' columns index topics. \code{sv} is a vector of singular values.
#' @details Latent semantic analysis, LSA, uses single value decomposition to
#' factor the document term matrix. In many LSA applications, TF-IDF weights are
#' applied to the DTM before model fitting. However, this is not strictly 
#' necessary. 
#' @examples
#' # Load a pre-formatted dtm 
#' data(nih_sample_dtm) 
#' 
#' # Convert raw word counts to TF-IDF frequency weights
#' idf <- log(nrow(nih_sample_dtm) / Matrix::colSums(nih_sample_dtm > 0))
#' 
#' dtm_tfidf <- Matrix::t(nih_sample_dtm) * idf
#' 
#' dtm_tfidf <- Matrix::t(dtm_tfidf)
#' 
#' # Fit an LSA model
#' model <- FitLsaModel(dtm = dtm_tfidf, k = 5)
#' 
#' str(model)
#' 
#' @export
FitLsaModel <- function(dtm, k, calc_coherence = TRUE, 
                        return_data = FALSE, 
                        return_all = FALSE, ...){
  
  opts <- list(...)
  
  # Fit LSA using single value decomposition on sparse matrices
  lsa <- RSpectra::svds(A = dtm, k = k, opts = opts)
  
  # Rename/transform objects so they conform to the convention in textmineR
  names(lsa)[ names(lsa) == "v" ] <- "phi"
  lsa$phi <- t(lsa$phi)
  colnames(lsa$phi) <- colnames(dtm)
  rownames(lsa$phi) <- paste("t", 1:nrow(lsa$phi), sep="_")
  
  names(lsa)[ names(lsa) == "u" ] <- "theta"
  rownames(lsa$theta) <- rownames(dtm)
  colnames(lsa$theta) <- rownames(lsa$phi)
  
  names(lsa)[ names(lsa) == "d" ] <- "sv"
  names(lsa$sv) <- names(lsa$theta)
  
  # get gamma for predictions
  lsa$gamma <- diag(lsa$sv) %*% lsa$phi
  lsa$gamma <- t(MASS::ginv(lsa$gamma))
  
  colnames(lsa$gamma) <- colnames(lsa$phi)
  rownames(lsa$gamma) <- rownames(lsa$phi)
  
  # calc coherence if desired
  if (calc_coherence) {
    lsa$coherence <- CalcProbCoherence(lsa$phi, dtm)
  }
  
  
  # keep dtm/tcm for predict method etc.
  if (return_data) {
    lsa$data <- dtm
  }
  
  if(! return_all ){
    lsa$niter <- NULL
    lsa$nops <- NULL
  }
  
  class(lsa) <- "lsa_topic_model"
  
  return(lsa)
  
}

#' Predict method for LSA topic models
#' @description Obtains predictions of topics for new documents from a fitted LSA model
#' @param object a fitted object of class "lsa_topic_model"
#' @param newdata a DTM or TCM of class dgCMatrix or a numeric vector
#' @param ... further arguments passed to or from other methods.
#' @return a "theta" matrix with one row per document and one column per topic
#' @examples
#' # Load a pre-formatted dtm 
#' data(nih_sample_dtm) 
#' 
#' # Convert raw word counts to TF-IDF frequency weights
#' idf <- log(nrow(nih_sample_dtm) / Matrix::colSums(nih_sample_dtm > 0))
#' 
#' dtm_tfidf <- Matrix::t(nih_sample_dtm) * idf
#' 
#' dtm_tfidf <- Matrix::t(dtm_tfidf)
#' 
#' # Fit an LSA model on the first 50 documents
#' model <- FitLsaModel(dtm = dtm_tfidf[1:50,], k = 5)
#' 
#' # Get predictions on the next 50 documents
#' pred <- predict(model, dtm_tfidf[51:100,])
#' @export
predict.lsa_topic_model <- function(object, newdata, ...) {
  
  ### Check inputs ----
  
  ## Add check for single row of dtm that gets coerced to a numeric by R
  
  if (class(object) != "lsa_topic_model") {
    stop("object must be a topic model object of class lsa_topic_model")
  }
  
  if (sum(c("dgCMatrix", "numeric") %in% class(newdata)) < 1) {
    stop("newdata must be a matrix of class dgCMatrix or a numeric vector")
  }
  
  if (class(newdata) == "numeric") { # if newdata is a numeric vector, assumed to be 1 document
    if (is.null(names(newdata))) {
      stop("it looks like newdata is a numeric vector without names. 
           Did you mean to pass a single document?
           If so, it needs a names attribute to index tokens")
    }
    
    # align vocabulary
    intersect_names <- intersect(names(newdata), colnames(object$gamma))
    
    if (length(intersect_names) == 0)
      stop("No common vocabulary terms between model and newdata. 
           See `intersect(names(newdata), colnames(object$gamma))`")
    
    newdata <- newdata[intersect_names]
    
    # coerce into a dgCMatrix
    newdata <- Matrix::Matrix(newdata, nrow = 1, sparse = TRUE)
    
    colnames(newdata) <- intersect_names
    
  } else {
      intersect_names <- intersect(colnames(newdata), colnames(object$gamma))
  }
  

  ### get predictions ----
  out <- newdata[,intersect_names] %*% t(object$gamma[,intersect_names])
  
  out <- as.matrix(out)
  
  return(out)
  
}

# Turn a document term matrix into a list for LDA Gibbs sampling
# This is an internal function, not exported to users
initialize_topic_counts <- function(dtm, k, alpha, beta, phi_initial = NULL, 
                                    theta_initial = NULL, freeze_topics = FALSE, 
                                    ...) {
  
  # check inputs
  
  # initialize phi if not already specified
  # this phi is used to sample topics for inital counts in the C++ function
  if (is.null(phi_initial)) {
    # phi_initial <- gtools::rdirichlet(n = k, alpha = beta)
    
    phi_initial <- apply(beta, 1, function(x){
      gtools::rdirichlet(n = 1, alpha = x)
    })
    
    phi_initial <- t(phi_initial)
  }
  
  # initialize theta if not already specified
  # if not specified (e.g. if this is a new model) make a matrix by sampling
  # from alpha. 
  if (is.null(theta_initial)) {
    
    theta_initial <- gtools::rdirichlet(n = nrow(dtm), alpha = alpha)
    
  }
  
  # initalize Cd by sampling from theta_initial. 
  # for asymmetric alpha, encodes more/less probable topics
  # we don't need to initialize Cv because we can use the probabilities in phi, 
  # along with our sampled Cd to do a single Gibbs iteration to populate all three
  # of Cd, Ck, and Cv
  cd_sampler <- function(size, prob){
    stats::rmultinom(n = 1, size = size, prob = prob)
  }
  
  if (nrow(dtm) <= 3000) { # if a small corpus, do sequential
    
    # Cd_start <- sapply(X = Matrix::rowSums(dtm),
    #                    FUN = cd_sampler)
    
    Cd_start <- mapply(FUN = cd_sampler,
                       size = Matrix::rowSums(dtm),
                       prob = as.list(data.frame(t(theta_initial)))) 
    
    Cd_start <- t(Cd_start)
    
  } else { # otherwise do it in parallel
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    iterator <- textmineR::TmParallelApply(batches, function(b){
      
      rows <- b:min(b + 2999, nrow(dtm))
      
      if (length(rows) > 1) {
        
        size <- Matrix::rowSums(dtm[rows, ])
        
        prob <- as.list(data.frame(t(theta_initial[rows, ])))
        
      } else {
        size <- sum(dtm[rows, ])
        
        prob <- theta_initial[rows, ]
      }
      
      list(size = size, prob = prob)
      
    }, export = c("dtm", "theta_initial"),
    ...)
    
    Cd_start <- textmineR::TmParallelApply(X = iterator,
                                           FUN = function(x){
                                             out <- mapply(FUN = cd_sampler,
                                                           size = x$size,
                                                           prob = x$prob)
                                             
                                             t(out)
                                           },
                                           export = "cd_sampler",
                                           ...)
    
    Cd_start <- do.call(rbind, Cd_start)
    
  }
  
  # Initialize objects with that single Gibbs iteration mentioned above
  if(nrow(dtm) > 3000){  # if we have more than 3,000 docs, do it in parallel
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    lexicon <- textmineR::TmParallelApply(batches, function(b){
      
      rows <- b:min(b + 2999, nrow(dtm))
      
      l <- create_lexicon(Cd = Cd_start[rows,],
                          Phi = phi_initial,
                          dtm = dtm[rows,],
                          alpha = alpha,
                          freeze_topics = freeze_topics)
      
    }, 
    export = c("alpha", "Cd_start", "phi_initial", "dtm", "freeze_topics"),
    ...)
    
    # combine 
    Zd <- Reduce("c", lapply(lexicon, function(l) l$Zd))
    
    docs <- Reduce("c", lapply(lexicon, function(l) l$docs))
    
    Cv <- Reduce("+", lapply(lexicon, function(l) l$Cv))
    
    Ck <- Reduce("+", lapply(lexicon, function(l) l$Ck))
    
    Cd <- do.call(rbind, lapply(lexicon, function(l) l$Cd))
    
    out <- list(docs = docs,
                Zd = Zd,
                Cd = Cd,
                Cv = Cv,
                Ck = Ck)
    
  }else{ # if we have 3,000 or fewer docs do it sequentially
    out <- create_lexicon(Cd = Cd_start,
                          Phi = phi_initial,
                          dtm = dtm,
                          alpha = alpha,
                          freeze_topics = freeze_topics)
  }
  
  out
  
}


#' Fit a Latent Dirichlet Allocation topic model
#' @description Fit a Latent Dirichlet Allocation topic model using collapsed Gibbs sampling. 
#' @param dtm A document term matrix or term co-occurrence matrix of class dgCMatrix.
#' @param k Integer number of topics.
#' @param iterations Integer number of iterations for the Gibbs sampler to run. 
#' @param burnin Integer number of burnin iterations. If \code{burnin} is greater than -1,
#'        the resulting "phi" and "theta" matrices are an average over all iterations
#'        greater than \code{burnin}.
#' @param alpha Numeric scalar or vector of length \code{k}. This is the prior 
#'        for topics over documents.
#' @param beta Numeric scalar, numeric vector of length \code{ncol(dtm)}, 
#'        or numeric matrix with \code{k} rows and \code{ncol(dtm)} columns.
#'        This is the prior for words over topics.
#' @param optimize_alpha Logical. Do you want to optimize alpha every iteration?
#'        Defaults to \code{FALSE}. See 'details' below for more information.
#' @param calc_likelihood Logical. Do you want to calculate the log likelihood every iteration?
#'        Useful for assessing convergence. Defaults to \code{FALSE}. 
#' @param calc_coherence Logical. Do you want to calculate probabilistic coherence of topics
#'        after the model is trained? Defaults to \code{FALSE}. This calls
#'        \code{\link[textmineR]{CalcProbCoherence}}.
#' @param calc_r2 Logical. Do you want to calculate R-squared after the model is trained?
#'        Defaults to \code{FALSE}. This calls \code{\link[textmineR]{CalcTopicModelR2}}.
#' @param return_data Logical. Do you want \code{dtm} returned as part of the model object?
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}
#' @return Returns an S3 object of class c("lda_topic_model"). 
#' @details This function calls a collapsed Gibbs sampler for Latent Dirichlet Allocation
#'   written using the excellent Rcpp package. Some implementation notes follow:
#'   
#'   When you use burn-in iterations (i.e. \code{burnin = TRUE}), the resulting 
#'   \code{phi} and \code{theta} matrices are calculated by averaging over every 
#'   iteration after the specified  number of burn-in iterations. If you do not 
#'   use burn-in iterations, then the matrices are calculated from the last run
#'   only. Ideally, you'd burn in every iteration before convergence, then average
#'   over the chain after its converved (and thus every observation is independent).
#'   
#'   If you set \code{optimize_alpha} to \code{TRUE}, then each element of \code{alpha}
#'   is proportional to the number of times each topic has be sampled that iteration
#'   averaged with the value of \code{alpha} from the previous iteration. This lets
#'   you start with a symmetric \code{alpha} and drift into an asymmetric one. 
#'   However, (a) this probably means that convergence will take longer to happen 
#'   or convergence may not happen at all. And (b) I make no guarantees that doing this
#'   will give you any benefit or that it won't hurt your model. Caveat emptor!
#'   
#'   The log likelihood calculation is the same that can be found on page 9 of
#'   \url{https://arxiv.org/pdf/1510.08628.pdf}. The only difference is that the
#'   version in \code{\link[textmineR]{textmineR}} allows \code{beta} to be a
#'   vector or matrix. (Vector used in this function, matrix used for model
#'   updates in \code{\link[textmineR]{update.lda_topic_model}}. At present, the 
#'   log likelihood function appears to be ok for assessing convergence. i.e. It 
#'   has the right shape. However, it is, as of this writing, returning positive
#'   numbers, rather than the expected negative numbers. Looking into that, but 
#'   in the meantime caveat emptor once again.
#'   
#' @examples 
#' # load some data
#' data(nih_sample_dtm)
#' 
#' # fit a model 
#' set.seed(12345)
#' m <- fit_lda_model(dtm = nih_sample_dtm[1:20,], k = 5,
#'                  iterations = 200, burnin = 175)
#'
#' str(m)
#' 
#' # predict on held-out documents using gibbs sampling "fold in"
#' p1 <- predict(m, nih_sample_dtm[21:100,], method = "gibbs",
#'               iterations = 200, burnin = 175)
#' 
#' # predict on held-out documents using the dot product method
#' p2 <- predict(m, nih_sample_dtm[21:100,], method = "dot")
#'
#' # compare the methods
#' barplot(rbind(p1[1,],p2[1,]), beside = TRUE, col = c("red", "blue")) 
#' @export
fit_lda_model <- function(dtm, k, iterations = NULL, burnin = -1, alpha = 0.1, beta = 0.05, 
                          optimize_alpha = FALSE, calc_likelihood = FALSE, 
                          calc_coherence = FALSE, calc_r2 = FALSE, 
                          return_data = FALSE, ...) {
  
  ### check validity of inputs ----
  
  # iterations and burnin acceptable?
  if (burnin >= iterations) {
    stop("burnin must be less than iterations")
  }
  
  # dtm of the correct format?
  if (! "dgCMatrix" %in% class(dtm)) {
    message("dtm is not of class dgCMatrix, attempting to convert...")
    
    dtm <- try(methods::as(dtm, "dgCMatrix", strict = TRUE)) # requires Matrix in namespace
    
    if (! "dgCMatrix" %in% class(dtm))
      stop("conversion failed. Please pass an object of class dgCMatrix for dtm")
  }
  
  # is k formatted correctly?
  if (! is.numeric(k))
    stop("k must be an integer")
  
  k <- floor(k) # in case somebody is cheeky and passes a decimal
  
  # iterations?
  if (is.null(iterations))
    stop("You must specify number of iterations")
  
  # alpha and beta?
  if (! is.numeric(alpha) | sum(is.na(alpha)) > 0 | sum(alpha == 0) == length(alpha))
    stop("alpha must be a numeric scalar or vector of length 'k' with no missing 
          values and at least one non-zero value")
  
  if (length(alpha) == 1) {
    alpha <- numeric(k) + alpha
  } else if (length(alpha) != k){
    stop("alpha must be a scalar or vector of length 'k'")
  }
  
  if (! is.numeric(beta) | sum(is.na(beta)) > 0 | sum(beta == 0) == length(beta))
    stop("beta must be a numeric scalar, a numeric vector of length 'ncol(dtm)', or
         a numeric matrix with 'k' rows and 'ncol(dtm)' columns with no missing 
         values and at least one non-zero value.")
  
  if (length(beta) == 1) { # if beta is a scalar
    
    beta <- matrix(beta, nrow = k, ncol = ncol(dtm))
    
    beta_class <- "scalar"
    
  } else if (is.vector(beta)){ # if beta is a vector
    
    if (length(beta) != ncol(dtm)) # if you didn't specify this vector right
      stop("beta must be a numeric scalar, a numeric vector of length 'ncol(dtm)', or
         a numeric matrix with 'k' rows and 'ncol(dtm)' columns with no missing 
         values and at least one non-zero value.")
    
    # otherwise let's carry on...
    # make beta a matrix to format for C++ funciton
    beta <- t(beta + matrix(0, nrow = length(beta), ncol = k))
    
    beta_class <- "vector"
    
  } else if (is.matrix(beta)) { # if beta is a matrix
    
    beta_class <- "matrix"
    
  } else { # if beta is of an un supported data type
    
    stop("beta must be a numeric scalar, a numeric vector of length 'ncol(dtm)', or
         a numeric matrix with 'k' rows and 'ncol(dtm)' columns with no missing 
         values and at least one non-zero value.")
    
  }
  
  if (! is.logical(calc_coherence))
    stop("calc_coherence must be logical")
  
  if (! is.logical(calc_r2))
    stop("calc_r2 must be logical")
  
  ### format inputs ----
  
  
  # other formatting
  counts <- initialize_topic_counts(dtm = dtm, k = 10, 
                                    alpha = alpha, beta = beta)
  
  
  ### run C++ gibbs sampler ----
  lda <- fit_lda_c(docs = counts$docs,
                   Nk = k,
                   alpha = alpha,
                   beta = beta,
                   Cd = counts$Cd,
                   Cv = counts$Cv,
                   Ck = counts$Ck,
                   Zd = counts$Zd,
                   Phi = counts$Cv, # this is actually ignored as freeze_topics = FALSE for initial fitting
                   iterations = iterations,
                   burnin = burnin,
                   freeze_topics = FALSE, # this stays FALSE for initial fitting 
                   calc_likelihood = calc_likelihood, 
                   optimize_alpha = optimize_alpha) 
  
  ### format posteriors correctly ----
  if (burnin > -1) { # if you used burnin iterations use Cd_mean etc.
    
    phi <- lda$Cv_mean + beta
    
    theta <- t(t(lda$Cd_mean) + alpha)
    
  } else { # if you didn't use burnin use standard counts (Cd etc.)
    
    phi <- lda$Cv + beta
    
    theta <- t(t(lda$Cd) + alpha)
    
  }
  
  phi <- phi / rowSums(phi)
  
  phi[is.na(phi)] <- 0 # just in case of a numeric issue
  
  theta <- theta / rowSums(theta)
  
  theta[is.na(theta)] <- 0 # just in case of a numeric issue
  
  colnames(phi) <- colnames(dtm)
  
  rownames(phi) <- seq_len(k) # changed from previous: paste0("t_", seq_len(Nk))
  
  colnames(theta) <- rownames(phi)
  
  rownames(theta) <- rownames(dtm)
  
  ### collect the results ----
  gamma <- CalcGamma(phi = phi, theta = theta, 
                     p_docs = Matrix::rowSums(dtm))
  
  names(lda$alpha) <- rownames(phi)
  
  colnames(lda$beta) <- colnames(phi)
  
  if (beta_class == "scalar") {
    
    beta_out <- beta[1, 1]
    
  } else if (beta_class == "vector") {
    
    beta_out <- beta[1, ]
    
  } else if (beta_class == "matrix") {
    
    beta_out <- beta
    
  } else { # this should be impossible, but science is hard and I am dumb.
    beta_out <- beta
    
    message("something went wrong with 'beta'. This isn't your fault. Please 
            contact Tommy at jones.thos.w[at]gmail.com and tell him to fix it.")
  }
  
  result <- list(phi = phi,
                 theta = theta,
                 gamma = gamma,
                 alpha = lda$alpha,
                 beta = beta_out,
                 log_likelihood = data.frame(iteration = lda$log_likelihood[1,],
                                             log_likelihood = lda$log_likelihood[2, ])
  ) # add other things here if necessary
  
  class(result) <- "lda_topic_model"
  
  ### calculate and add other things ---
  if (calc_coherence) {
    result$coherence <- CalcProbCoherence(result$phi, dtm)
  }
  
  if (calc_r2) {
    result$r2 <- CalcTopicModelR2(dtm, result$phi, result$theta, ...)
  }
  
  if (! calc_likelihood) {
    result$log_likelihood <- NULL
  }
  
  ### return the final result ----
  result
  
}

### Predict method for LDA objects
#' Get predictions from a Latent Dirichlet Allocation model
#' @description Obtains predictions of topics for new documents from a fitted LDA model
#' @param object a fitted object of class \code{lda_topic_model}
#' @param newdata a DTM or TCM of class \code{dgCMatrix} or a numeric vector
#' @param method one of either "gibbs" or "dot". If "gibbs" Gibbs sampling is used
#'        and \code{iterations} must be specified.
#' @param iterations If \code{method = "gibbs"}, an integer number of iterations 
#'        for the Gibbs sampler to run. A future version may include automatic stopping criteria.
#' @param burnin If \code{method = "gibbs"}, an integer number of burnin iterations. 
#'        If \code{burnin} is greater than -1, the entries of the resulting "theta" matrix
#'        are an average over all iterations greater than \code{burnin}.
#'        Behavior is the same as documented in \code{\link[textmineR]{FitLdaTopicModel}}. 
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}
#' @return a "theta" matrix with one row per document and one column per topic
#' @examples
#' \dontrun{
#' # load some data
#' data(nih_sample_dtm)
#' 
#' # fit a model 
#' set.seed(12345)
#' 
#' m <- FitLdaModel(dtm = nih_sample_dtm[1:20,], k = 5,
#'                  iterations = 200, burnin = 175)
#'
#' str(m)
#' 
#' # predict on held-out documents using gibbs sampling "fold in"
#' p1 <- predict(m, nih_sample_dtm[21:100,], method = "gibbs",
#'               iterations = 200, burnin = 175)
#' 
#' # predict on held-out documents using the dot product method
#' p2 <- predict(m, nih_sample_dtm[21:100,], method = "dot")
#'
#' # compare the methods
#' barplot(rbind(p1[1,],p2[1,]), beside = TRUE, col = c("red", "blue")) 
#' }
#' @export
predict.lda_topic_model <- function(object, newdata, method = c("gibbs", "dot"), 
                                    iterations = NULL, burnin = -1, ...) {
  
  ### Check inputs ----
  if (method[1] == "gibbs") {
    
    if (is.null(iterations)) {
      stop("when using method 'gibbs' iterations must be specified.")
    }
    
    if (burnin >= iterations) {
      stop("burnin must be less than iterations")
    }
    
  }
  
  
  if (class(object) != "lda_topic_model") {
    stop("object must be a topic model object of class lda_topic_model")
  }
  
  if (sum(c("dgCMatrix", "numeric") %in% class(newdata)) < 1) {
    stop("newdata must be a matrix of class dgCMatrix or a numeric vector")
  }
  
  if (class(newdata) == "numeric") { # if newdata is a numeric vector, assumed to be 1 document
    if (is.null(names(newdata))) {
      stop("it looks like newdata is a numeric vector without names. 
           Did you mean to pass a single document?
           If so, it needs a names attribute to index tokens")
    }
    
    
    vocab <- names(newdata)
    
    newdata <- Matrix::Matrix(newdata, nrow = 1, sparse = TRUE)
    
    colnames(newdata) <- vocab
    
    rownames(newdata) <- 1
    
  }
  
  if (sum(c("gibbs", "dot") %in% method) == 0) {
    stop("method must be one of 'gibbs' or 'dot'")
  }
  
  dtm_newdata <- newdata
  
  ### Align vocabulary ----
  # this is fancy because of how we do indexing in gibbs sampling
  vocab_original <- colnames(object$phi) # tokens in training set
  
  vocab_intersect <- intersect(vocab_original, colnames(dtm_newdata))
  
  vocab_add <- setdiff(vocab_original, vocab_intersect)
  
  add_mat <- Matrix::Matrix(0, nrow = nrow(dtm_newdata), ncol = length(vocab_add))
  
  colnames(add_mat) <- vocab_add
  
  dtm_newdata <- Matrix::cbind2(dtm_newdata, add_mat)
  
  if (nrow(dtm_newdata) == 1) {
    dtm_newdata <- Matrix::Matrix(dtm_newdata[,vocab_original], nrow = 1, sparse = TRUE)
    
    colnames(dtm_newdata) <- vocab_original
    
    rownames(dtm_newdata) <- 1
  } else {
    
    dtm_newdata <- dtm_newdata[, vocab_original]
    
  }
  
  ### Get predictions ----
  
  if (method[1] == "dot") { # dot product method
    
    result <- dtm_newdata[, vocab_original]
    
    # handle differently if one row
    if (nrow(dtm_newdata) == 1) {
      result <- result / sum(result)
    } else {
      result <- result / Matrix::rowSums(result)
    }
    
    result <- result %*% t(object$gamma[ ,vocab_original])
    result <- as.matrix(result)
    result[is.na(result)] <- 0
    
  } else { # gibbs method
    # format inputs
    
    # get initial distribution with recursive call to "dot" method
    theta_initial <- predict.lda_topic_model(object = object, newdata = newdata, method = "dot")
    
    lex <- initialize_topic_counts(dtm = dtm_newdata,
                                   k = nrow(object$phi),
                                   alpha = object$alpha,
                                   beta = object$beta,
                                   phi_initial = object$phi,
                                   theta_initial = theta_initial,
                                   freeze_topics = TRUE)
    
    # pass inputs to C function
    theta <- fit_lda_c(docs = lex$docs,
                       Nk = nrow(object$phi),
                       beta = t(object$beta + matrix(0, nrow = length(object$beta), 
                                                     ncol = length(object$alpha))),
                       alpha = object$alpha,
                       Cd = lex$Cd,
                       Cv = lex$Cv,
                       Ck = lex$Ck,
                       Zd = lex$Zd,
                       Phi = object$phi,
                       iterations = iterations,
                       burnin = burnin,
                       freeze_topics = TRUE,
                       calc_likelihood = FALSE,
                       optimize_alpha = FALSE)
    
    # format posterior prediction
    if (burnin > -1) { # if you used burnin iterations use Cd_mean etc.
      
      theta <- t(t(theta$Cd_mean) + object$alpha)
      
    } else { # if you didn't use burnin use standard counts (Cd etc.)
      
      theta <- t(t(theta$Cd) + object$alpha)
      
    }
    
    theta <- theta / rowSums(theta, na.rm = TRUE)
    
    theta[ is.na(theta) ] <- 0
    
    result <- theta
  }
  
  # return result
  
  rownames(result) <- rownames(dtm_newdata)
  colnames(result) <- rownames(object$phi)
  
  result
  
}

#' Update a Latent Dirichlet Allocation topic model
#' @description Update an LDA model using collapsed Gibbs sampling. 
#' @param object a fitted object of class \code{lda_topic_model}.
#' @param dtm A document term matrix or term co-occurrence matrix of class dgCMatrix.
#' @param additional_k Integer number of topics to add, defaults to 0.
#' @param phi_as_prior Logical. Do you want to replace \code{beta} with \code{phi}
#'        from the previous model as the prior for words over topics?
#' @param iterations Integer number of iterations for the Gibbs sampler to run. 
#' @param burnin Integer number of burnin iterations. If \code{burnin} is greater than -1,
#'        the resulting "phi" and "theta" matrices are an average over all iterations
#'        greater than \code{burnin}.
#' @param optimize_alpha Logical. Do you want to optimize alpha every iteration?
#'        Defaults to \code{FALSE}. See 'details' of documentation for
#'        \code{\link[textmineR]{FitLdaModel}}for more information.
#' @param calc_likelihood Logical. Do you want to calculate the log likelihood every iteration?
#'        Useful for assessing convergence. Defaults to \code{FALSE}. 
#' @param calc_coherence Logical. Do you want to calculate probabilistic coherence of topics
#'        after the model is trained? Defaults to \code{FALSE}. This calls
#'        \code{\link[textmineR]{CalcProbCoherence}}.
#' @param calc_r2 Logical. Do you want to calculate R-squared after the model is trained?
#'        Defaults to \code{FALSE}. This calls \code{\link[textmineR]{CalcTopicModelR2}}.
#' @param return_data Logical. Do you want \code{dtm} returned as part of the model object?
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}
#' @return Returns an S3 object of class c("lda_topic_model"). 
#' @details 
#' prior + counts vs. counts only. Vocab alignment + uniform prior over new words. 
#'          Adding additional topics. works best with significant vocab overlap
#' @export
#' @examples 
#' \dontrun{
#' # load a document term matrix
#' d1 <- nih_sample_dtm[1:50,]
#' 
#' d2 <- nih_sample_dtm[51:100,]
#' 
#' # fit a model
#' m <- FitLdaModel(d1, k = 10, 
#'                  iterations = 200, burnin = 175,
#'                  optimize_alpha = TRUE, 
#'                  calc_likelihood = FALSE,
#'                  calc_coherence = TRUE,
#'                  calc_r2 = FALSE)
#' 
#' # update an existing model by adding documents
#' m2 <- update(object = m,
#'              dtm = rbind(d1, d2),
#'              iterations = 200,
#'              burnin = 175)
#'              
#' # use an old model as a prior for a new model
#' m3 <- update(object = m,
#'              dtm = d2, # new documents only
#'              iterations = 200,
#'              burnin = 175)
#'              
#' # add topics while updating a model by adding documents
#' m4 <- update(object = m,
#'              dtm = rbind(d1, d2),
#'              additional_k = 3,
#'              iterations = 200,
#'              burnin = 175)
#'              
#' # add topics to an existing model
#' m5 <- update(object = m,
#'              dtm = d1, # this is the old data
#'              additional_k = 3,
#'              iterations = 200,
#'              burnin = 175)
#' 
#' }
update.lda_topic_model <- function(object, dtm, additional_k = 0, 
                                   phi_as_prior = FALSE,
                                   iterations = NULL, burnin = -1, 
                                   optimize_alpha = FALSE, calc_likelihood = FALSE, 
                                   calc_coherence = FALSE, calc_r2 = FALSE, 
                                   return_data = FALSE, ...) {
  
  ### Check inputs are of correct dimensionality ----
  
  # object of correct class?
  if (class(object) != "lda_topic_model")
    stop("object must be of class lda_topic_model")
  
  # iterations and burnin acceptable?
  if (burnin >= iterations) {
    stop("burnin must be less than iterations")
  }
  
  # dtm of the correct format?
  if (! "dgCMatrix" %in% class(dtm)) {
    message("'dtm' is not of class dgCMatrix, attempting to convert...")
    
    dtm <- try(methods::as(dtm, "dgCMatrix", strict = TRUE)) # requires Matrix in namespace
    
    if (! "dgCMatrix" %in% class(dtm))
      stop("conversion failed. Please pass an object of class dgCMatrix for dtm")
  }
  
  # is k formatted correctly?
  if (! is.numeric(additional_k)){
    stop("additional_k must be an integer >= 0")
  } else if (additional_k < 0) {
    stop("additional_k must be an integer >= 0")
  }
  
  additional_k <- floor(additional_k) # in case somebody is cheeky and passes a decimal
  
  # iterations?
  if (is.null(iterations))
    stop("You must specify number of iterations")
  
  if (! is.logical(calc_coherence))
    stop("calc_coherence must be TRUE or FALSE")
  
  if (! is.logical(calc_r2))
    stop("calc_r2 must be TRUE or FALSE")
  
  if (! is.logical(calc_likelihood))
    stop("calc_likelihood must be TRUE or FALSE")
  
  if (! is.logical(phi_as_prior))
    stop("phi_as_prior must be TRUE or FALSE")
  
  
  ### Pull out objects used for update ----
  
  # format of beta
  if (phi_as_prior) {
    
    beta <- object$phi
    
    beta_class <- "matrix"
    
    # re-scale so that beta has the same magnitude of the old beta
    
    if (is.matrix(object$beta)) {
      
      beta <- beta * rowSums(object$beta)
      
    } else if (is.vector(object$beta)) {
      
      beta <- beta * sum(object$beta)
      
    } else if (length(object$beta) == 1) {
      
      beta <- beta * (object$beta * ncol(object$phi))
      
    } else { # this case shouldn't happen
      
      stop("object$beta must be a numeric scalar, a numeric vector of length 
         'ncol(object$phi)', or a numeric matrix with 'nrow(object$phi)' rows 
         and 'ncol(object$phi)' columns with no missing  values and at least 
         one non-zero value.")
      
    }
    
  } else {
    
    if (is.matrix(object$beta)) {
      
      beta <- object$beta
      
      beta_class <- "matrix"
      
    } else if (is.vector(object$beta)) {
      
      beta <- t(object$beta + matrix(0, nrow = length(object$beta), ncol = k))
      
      beta_class <- "vector"
      
    } else if (length(object$beta) == 1) {
      
      beta <- matrix(object$beta, nrow = k, ncol = ncol(dtm))
      
      beta_class <- "scalar"
      
    } else { # this condition should never happen...
      
      stop("object$beta must be a numeric scalar, a numeric vector of length 
         'ncol(object$phi)', or a numeric matrix with 'nrow(object$phi)' rows 
         and 'ncol(object$phi)' columns with no missing  values and at least 
         one non-zero value.")
      
    }
    
  }
  
  dimnames(beta) <- dimnames(object$phi)
  
  # phi_initial and theta_initial
  phi_initial <- object$phi
  
  theta_initial <- predict.lda_topic_model(object = object,
                                           newdata = dtm,
                                           method = "dot")
  
  # pull out alpha
  alpha <- object$alpha
  
  ### Vocabulary alignment and new topic (if any) alignment ----
  
  # align vocab in intelligent way for adding new vocab
  v_diff <- setdiff(colnames(dtm), colnames(phi_initial))
  
  m_add <- matrix(0, nrow = nrow(phi_initial), ncol = length(v_diff))
  
  colnames(m_add) <- v_diff
  
  beta <- cbind(beta, m_add+ median(beta)) # uniform prior over new words
  
  beta <- beta[, colnames(dtm)]
  
  phi_initial <- cbind(phi_initial, m_add + median(phi_initial)) 
  
  phi_initial <- phi_initial[, colnames(dtm)] / rowSums(phi_initial[, colnames(dtm)])
  
  # add topics to beta and phi_initial
  # prior for topics inherets from beta, specifically colMeans(beta)
  # basically means that new topics are an average of old topics. If you used
  # a scalar or vector for object$beta, then prior for new topics will be 
  # identical to prior for old topics. If object$beta was a matrix where rows
  # were not identical (i.e. you seeded specific topics), then your new topics
  # will have a prior that is the average of all old topics.
  m_add <- matrix(0, 
                  nrow = additional_k, 
                  ncol = ncol(beta))
  
  m_add <- t(t(m_add) + colMeans(beta)) 
  
  beta <- rbind(beta, m_add) # add new topics to beta
  
  phi_initial <- rbind(phi_initial, m_add / rowSums(m_add)) # new topics to phi
  
  # add topics to alpha and theta_initial
  # prior for new topics is uniform, similar to beta, it's the median of alpha
  # adding new topics to theta_inital is a little more complicated. We take the
  # median of each row of theta_initial, add that to the new topics and then
  # reweight so each row still sums to 1.
  alpha <- c(alpha, rep(median(alpha), additional_k)) # uniform prior for new topics
  
  m_add <- apply(theta_initial, 1, function(x){
    rep(median(x), additional_k)
  })
  
  # handle cases on what m_add could be
  if (is.matrix(m_add)) { # if we add more than one topic
    
    m_add <- t(m_add)
    
    colnames(m_add) <- (max(as.numeric(colnames(theta_initial))) + 1):
      (max(as.numeric(colnames(theta_initial))) + additional_k)
    
    theta_initial <- cbind(theta_initial, m_add)
    
    theta_initial <- theta_initial / rowSums(theta_initial)
    
    
  } else if (length(m_add) == 0) { # we add no topics and get nothing back
    
    # do nothing, actually
    
  } else { # we add only one topic and get a vector back
    
    theta_initial <- cbind(theta_initial, m_add)
    
    theta_initial <- theta_initial / rowSums(theta_initial)
    
  }
  
  
  ### get initial counts to feed to gibbs sampler ----
  counts <- initialize_topic_counts(dtm = dtm, 
                                    k = nrow(phi_initial),
                                    alpha = alpha,
                                    beta = beta,
                                    phi_initial = phi_initial,
                                    theta_initial = theta_initial,
                                    freeze_topics = FALSE) # false because this is an update
  
  ### run C++ gibbs sampler ----
  lda <- fit_lda_c(docs = counts$docs,
                   Nk = nrow(phi_initial),
                   alpha = alpha,
                   beta = beta,
                   Cd = counts$Cd,
                   Cv = counts$Cv,
                   Ck = counts$Ck,
                   Zd = counts$Zd,
                   Phi = counts$Cv, # this is actually ignored as freeze_topics = FALSE 
                   iterations = iterations,
                   burnin = burnin,
                   freeze_topics = FALSE, # this stays FALSE for updates 
                   calc_likelihood = calc_likelihood, 
                   optimize_alpha = optimize_alpha) 
  
  
  
  ### Format posteriors correctly ----
  if (burnin > -1) { # if you used burnin iterations use Cd_mean etc.
    
    phi <- lda$Cv_mean + beta
    
    theta <- t(t(lda$Cd_mean) + alpha)
    
  } else { # if you didn't use burnin use standard counts (Cd etc.)
    
    phi <- lda$Cv + beta
    
    theta <- t(t(lda$Cd) + alpha)
    
  }
  
  phi <- phi / rowSums(phi)
  
  phi[is.na(phi)] <- 0 # just in case of a numeric issue
  
  theta <- theta / rowSums(theta)
  
  theta[is.na(theta)] <- 0 # just in case of a numeric issue
  
  colnames(phi) <- colnames(dtm)
  
  rownames(phi) <- seq_len(nrow(phi)) 
  
  colnames(theta) <- rownames(phi)
  
  rownames(theta) <- rownames(dtm)
  
  ### collect the results ----
  gamma <- CalcGamma(phi = phi, theta = theta, 
                     p_docs = Matrix::rowSums(dtm))
  
  names(lda$alpha) <- rownames(phi)
  
  colnames(lda$beta) <- colnames(phi)
  
  if (beta_class == "scalar") {
    
    beta_out <- beta[1, 1]
    
  } else if (beta_class == "vector") {
    
    beta_out <- beta[1, ]
    
  } else if (beta_class == "matrix") {
    
    beta_out <- beta
    
  } else { # this should be impossible, but science is hard and I am dumb.
    beta_out <- beta
    
    message("something went wrong with 'beta'. This isn't your fault. Please 
            contact Tommy at jones.thos.w[at]gmail.com and tell him to fix it.")
  }
  
  result <- list(phi = phi,
                 theta = theta,
                 gamma = gamma,
                 alpha = lda$alpha,
                 beta = beta_out,
                 log_likelihood = data.frame(iteration = lda$log_likelihood[1,],
                                             log_likelihood = lda$log_likelihood[2, ])
  ) # add other things here if necessary
  
  class(result) <- "lda_topic_model"
  
  ### calculate and add other things ---
  if (calc_coherence) {
    result$coherence <- CalcProbCoherence(result$phi, dtm)
  }
  
  if (calc_r2) {
    result$r2 <- CalcTopicModelR2(dtm, result$phi, result$theta, ...)
  }
  
  if (! calc_likelihood) {
    result$log_likelihood <- NULL
  }
  
  ### return the final result ----
  result
  
}
