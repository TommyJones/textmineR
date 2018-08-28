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
CalcGamma <- function(phi, theta, p_docs = NULL){
  
  # set up constants
  D <- nrow(theta)
  K <- ncol(theta)
  V <- ncol(phi)
  
  # probability of each document (assumed to be equiprobable)
  if(is.null(p_docs)){
    p.d <- rep(1/nrow(theta), nrow(theta))
  }else{
    if(sum(is.na(p_docs)) > 0){
      warning("found missing values in p_docs. Setting them as 0.")
      p_docs[ is.na(p_docs) ] <- 0 
    }
    p.d <- p_docs / sum(p_docs)
  }
  
  # get the probability of each topic
  p.t <- p.d %*% theta
  
  # get the probability of each word from the model    
  p.w <- p.t %*% phi
  
  
  
  # get our result
  phi.prime <- matrix(0, ncol=ncol(p.t), nrow=ncol(p.t))
  diag(phi.prime) <- p.t
  
  phi.prime <- phi.prime %*% phi
  
  phi.prime <- t(apply(phi.prime, 1, function(x) x / p.w))
  
  rownames(phi.prime) <- rownames(phi)
  colnames(phi.prime) <- colnames(phi)
  
  return(phi.prime)
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
  
  return(list(theta = theta, phi = phi))
}


#' @title Fit a Correlated Topic Model
#' @description A wrapper for the \link[topicmodels]{CTM} function based on 
#' Blei's original code that returns a nicely-formatted topic model.
#' @param dtm A document term matrix of class \code{dgCMatrix}
#' @param k Number of topics
#' @param return_all Logical. Do you want the raw results of the underlying 
#' function returned along with the formatted results? Defaults to \code{TRUE}.
#' @param ... Other arguments to pass to \link[topicmodels]{CTM}. 
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
FitCtmModel <- function(dtm, k, return_all = TRUE, ...){
  
  model <- topicmodels::CTM(x = dtm, k = k, control = list(...))
  
  theta <- model@gamma
  
  rownames(theta) <- model@documents
  colnames(theta) <- paste("t", 1:ncol(theta), sep = "_")
  
  phi <- exp(model@beta)
  colnames(phi) <- model@terms
  rownames(phi) <- colnames(theta)
  
  result <- list(theta = theta, phi = phi)
  
  if(return_all){
    result <- c(result, etc = model)
  }
  
  return(result)
}


#' Fit a topic model using Latent Semantic Analysis
#' @description A wrapper for \code{RSpectra::svds} that returns 
#' a nicely-formatted latent semantic analysis topic model.
#' @param dtm A document term matrix of class \code{Matrix::dgCMatrix}
#' @param k Number of topics
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
FitLsaModel <- function(dtm, k, return_all = FALSE, ...){
  
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
  
  if(! return_all ){
    lsa$niter <- NULL
    lsa$nops <- NULL
  }
  
  return(lsa)
  
}

################################################################################
# This file turns the Gibbs sampler cobbled from various sources into an R function
# Once I am sure that it is (a) calculating properly and (b) efficient, I will
# roll it over to C++
################################################################################


#' Turn a document term matrix into a list for LDA Gibbs sampling
#' @description Represents a document term matrix as a list. 
#' @param dtm A document term matrix (or term co-occurrence matrix) of class 
#' \code{dgCMatrix}. 
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}.
#' @return Returns a list. Each element of the list represents a row of the input
#' matrix. Each list element contains a numeric vector with as many entries as
#' tokens in the original document. The entries are the column index for that token, minus 1. 
#' @examples
#' \dontrun{
#' # Load pre-formatted data for use
#' data(nih_sample_dtm)
#' 
#' result <- Dtm2Lexicon(dtm = nih_sample_dtm, 
#'                       cpus = 2)
#' }
#' @export 
Dtm2Lexicon <- function(dtm, ...) {
  
  # do in parallel in batches of about 3000 if we have more than 3000 docs
  if(nrow(dtm) > 3000){
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    dtm_list <- lapply(batches, function(x) dtm[ x:min(x + 2999, nrow(dtm)) , ])
    
    out <-textmineR::TmParallelApply(X = dtm_list, FUN = function(y){
      dtm_to_lexicon_c(x = y)
    }, ...)
    
    out <- do.call(c, out)
    
  }else{
    out <- dtm_to_lexicon_c(x = dtm)
  }
  
  names(out) <- rownames(dtm)
  
  out
  
}

#' Fit a Latent Dirichlet Allocation topic model
#' @description Fit a Latent Dirichlet Allocation topic model using collapsed Gibbs sampling. 
#' @param dtm A document term matrix or term co-occurrence matrix of class dgCMatrix
#' @param k Integer number of topics
#' @param alpha Vector of length \code{k} for asymmetric or a number for symmetric.
#'        This is the prior for topics over documents
#' @param beta Vector of length \code{ncol(dtm)} for asymmetric or a number for symmetric.
#'        This is the prior for words over topics.
#' @param iterations Integer number of iterations for the Gibbs sampler to run. A
#'        future version may include automatic stopping criteria.
#' @param seed If not null (the default) then the random seed you wish to set. This
#' will return the same outputs for the same inputs. Useful for diagnostics.
#' @param ... Other arguments to be passed to textmineR::TmParallelApply
#' @return Returns an S3 object of class c("LDA", "TopicModel"). DESCRIBE MORE
#' @details EXPLAIN IMPLEMENTATION DETAILS
#' @examples GIVE EXAMPLES
#' @export
FitLdaModel <- function(dtm, k, iterations = NULL, burnin = -1, alpha = 0.1, beta = 0.05, 
                        calc_coherence = TRUE, calc_r2 = FALSE, seed = NULL, ...){
  
  ### Check inputs are of correct dimensionality ----
  
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
  if (! is.numeric(alpha) | sum(is.na(alpha)) > 0)
    stop("alpha must be a numeric scalar or vector with no missing values")
  
  if (length(alpha) == 1) {
    alpha <- numeric(k) + alpha
  } else if (length(alpha) != k){
    stop("alpha must be a scalar or vector of length k")
  }
  
  if (! is.numeric(beta) | sum(is.na(beta)) > 0)
    stop("beta must be a numeric scalar or vector with no missing values")
  
  if (length(beta) == 1) {
    beta <- numeric(ncol(dtm)) + beta
  } else if (length(beta) != ncol(dtm)){
    stop("beta must be a scalar or vector of length ncol(dtm)")
  }
  
  if (! is.null(seed)){
    if (! is.numeric(seed)){
      stop("if seed is not NULL, then it must be numeric")
    } else {
      set.seed(seed)
    }
  }
  
  if (! is.logical(calc_coherence))
    stop("calc_coherence must be logical")
  
  if (! is.logical(calc_r2))
    stop("calc_r2 must be logical")
  
  
  ### Format inputs ----
  
  docs <- Dtm2Lexicon(dtm)
  
  Nd <- nrow(dtm)
  
  Nk <- k
  
  Nv <- ncol(dtm)
  
  ### run C++ gibbs sampler ----
  
  result <- fit_lda_c(docs = docs, Nk = Nk, Nd = Nd, Nv = Nv, 
                      alpha = alpha, beta = beta,
                      iterations = iterations, burnin = burnin)
  
  
  ### Format posteriors correctly ----
  
  phi <- result$phi
  
  theta <- result$theta
  
  phi <- t(t(phi) + beta)
  
  phi <- phi / rowSums(phi, na.rm = TRUE)
  
  phi[ is.na(phi) ] <- 0
  
  theta <- t(t(theta) + alpha)
  
  theta <- theta / rowSums(theta, na.rm = TRUE)
  
  theta[ is.na(theta) ] <- 0
  
  colnames(phi) <- colnames(dtm)
  
  rownames(phi) <- paste0("t_", seq_len(Nk))
  
  colnames(theta) <- rownames(phi)
  
  rownames(theta) <- rownames(dtm)
  
  ### collect the result ----
  gamma <- textmineR::CalcPhiPrime(phi = phi, theta = theta, 
                                   p_docs = Matrix::rowSums(dtm))
  
  result <- list(phi = phi, theta = theta, gamma = gamma,
                 dtm = dtm, alpha = alpha, beta = beta) # add other things here
  
  class(result) <- c("LDA", "TopicModel")
  
  ### calculate additional things ----
  if (calc_coherence) {
    result$coherence <- textmineR::CalcProbCoherence(result$phi, dtm, M = 5)
  }
  
  if (calc_r2) {
    result$r2 <- textmineR::CalcTopicModelR2(dtm, result$phi, result$theta, ...)
  }
  
  ### return result ----
  result
}

### Predict method for LDA objects
predict.LDA <- function(object, newdata, method = c("gibbs", "dot"), 
                        iterations = NULL, burnin = -1, seed = NULL, ...) {
  
  ### Check inputs ----
  if (method[1] == "gibbs") {
    
    if (is.null(iterations)) {
      stop("when using method 'gibs' iterations must be specified.")
    }
    
    if (burnin >= iterations) {
      stop("burnin must be less than iterations")
    }
    
  }
  
  
  if (sum(c("LDA", "TopicModel") %in% class(object)) < 2) {
    stop("object must be a topic model object of class c('LDA', 'TopicModel')")
  }
  
  if (sum(c("dgCMatrix", "character") %in% class(newdata)) < 1) {
    stop("newdata must be a matrix of class dgCMatrix or a character vector")
  }
  
  if (sum(c("gibbs", "dot") %in% method) == 0) {
    stop("method must be one of 'gibbs' or 'dot'")
  }
  
  if (! is.null(seed)) {
    if (! is.numeric(seed)){
      stop("seed must be NULL or numeric")
    }
    set.seed(seed)
  }
  
  ### If newdata is a character vector, convert to dgCMatrix ----
  # TODO: requires re-write of outputs of CreateDtm/CreateTcm
  
  if ("dgCMatrix" %in% class(newdata)) {
    dtm_newdata <- newdata
  } else {
    # code to convert goes here
    stop("newdata not of class character is not yet supported")
  }
  
  ### Align vocabulary ----
  vocab1 <- colnames(object$dtm)
  # vocab2 <- setdiff(colnames(dtm_newdata), colnames(object$dtm)) # for now unused
  
  ### Get predictions ----
  
  if (method[1] == "dot") { # dot product method
    
    result <- dtm_newdata[ ,vocab1]
    result <- (result / Matrix::rowSums(result)) %*% t(object$gamma[ ,vocab1])
    result <- as.matrix(result)
    
  } else { # gibbs method
    # format inputs
    docs <- Dtm2Lexicon(dtm_newdata[,vocab1])
    
    Nd <- nrow(dtm_newdata)
    
    Nk <- nrow(object$phi)
    
    sum_alpha <- sum(object$alpha)
    
    # pass inputs to C function
    theta <- predict_lda_c(docs = docs, Nk = Nk, Nd = Nd, 
                           alpha = object$alpha, phi = object$phi,
                           iterations = iterations, burnin = burnin)
    
    theta <- theta$theta
    
    # format outputs
    theta <- t(t(theta) + object$alpha)
    
    theta <- theta / rowSums(theta, na.rm = TRUE)
    
    theta[ is.na(theta) ] <- 0
    
    result <- theta
  }
  
  # return result
  
  rownames(result) <- rownames(dtm_newdata)
  colnames(result) <- rownames(object$phi)
  
  result
  
}




