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
  phi_prime <- matrix(0, ncol=ncol(p_t), nrow=ncol(p_t))
  diag(phi_prime) <- p_t
  
  phi_prime <- phi_prime %*% phi
  
  phi_prime <- t(apply(phi_prime, 1, function(x) x / p_w))
  
  rownames(phi_prime) <- rownames(phi)
  colnames(phi_prime) <- colnames(phi)
  
  return(phi_prime)
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
  
  # get gamma for predictions
  lsa$gamma <- diag(lsa$sv) %*% lsa$phi
  lsa$gamma <- t(MASS::ginv(lsa$gamma))
  
  colnames(lsa$gamma) <- colnames(lsa$phi)
  rownames(lsa$gamma) <- rownames(lsa$phi)
  
  # keep dtm/tcm for predict method etc.
  lsa$data <- dtm
  
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
#' @param newdata a DTM or TCM of class dgCMatrix or a character vector
#' @param verbose Defaults to \code{FALSE}. If \code{newdata} is a character vector,
#'        do you want to see status during vectorization?
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
predict.lsa_topic_model <- function(object, newdata, verbose = FALSE) {
  
  ### Check inputs ----
  
  ## Add check for single row of dtm that gets coerced to a numeric by R
  
  if (class(object) != "lsa_topic_model") {
    stop("object must be a topic model object of class lsa_topic_model")
  }
  
  if (sum(c("dgCMatrix", "character", "numeric") %in% class(newdata)) < 1) {
    stop("newdata must be a matrix of class dgCMatrix or a character vector")
  }
  
  if (class(newdata) == "numeric") { # if newdata is a numeric vector, assumed to be 1 document
    if (is.null(names(newdata))) {
      stop("it looks like newdata is a numeric vector without names. 
           Did you mean to pass a single document?
           If so, it needs a names attribute to index tokens")
    }
    
    new_names <- names(newdata)
    
    newdata <- Matrix::Matrix(newdata, nrow = 1)
    
    colnames(newdata) <- new_names
    
  }

  ### if newdata is a document vector, make a dtm ----
  if (class(newdata) == "character") {
    ## Add a check here to make sure it's the right object & return helpful error message
    
    data_args <- attr(object$data, "args")
    
    data_args$doc_vec <- newdata
    
    data_args$verbose <- verbose
    
    if (attr(object$data, "call") == "CreateDtm") {
      newdata <- do.call(CreateDtm, data_args)
    } else if (attr(object$data, "call") == "CreateTcm") {
      newdata <- do.call(CreateTcm, data_args)
    } else {
      stop("Something is wrong with object$data. Cannot find attribute 'call'.")
    }

  }
  
  ### align vocabulary ----
  vocab <- intersect(colnames(newdata), colnames(object$gamma))
  
  ### get predictions ----
  out <- newdata[,vocab] %*% t(object$gamma[,vocab])
  
  out <- as.matrix(out)
  
  return(out)
  
}

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
    
    out <- TmParallelApply(X = dtm_list, FUN = function(y){
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
#' @examples # GIVE EXAMPLES
#' @export
FitLdaModel <- function(dtm, k, iterations = NULL, burnin = -1, alpha = 0.1, beta = 0.05, 
                        optimize_alpha = FALSE, calc_likelihood = FALSE, 
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
                      alph = alpha, beta = beta,
                      iterations = iterations, burnin = burnin,
                      optimize_alpha = optimize_alpha,
                      calc_likelihood = calc_likelihood)
  
  
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
  gamma <- CalcGamma(phi = phi, theta = theta, 
                                   p_docs = Matrix::rowSums(dtm))
  
  result <- list(phi = phi, theta = theta, gamma = gamma,
                 data = dtm, alpha = result$alpha, beta = result$beta,
                 log_likelihood = data.frame(result$log_likelihood)) # add other things here
  
  names(result$log_likelihood) <- c("iteration", "log_likelihood")
  
  class(result) <- "lda_topic_model"
  
  ### calculate additional things ----
  if (calc_coherence) {
    result$coherence <- CalcProbCoherence(result$phi, dtm, M = 5)
  }
  
  if (calc_r2) {
    result$r2 <- CalcTopicModelR2(dtm, result$phi, result$theta, ...)
  }
  
  if (! calc_likelihood) {
    result$log_likelihood <- NULL
  }
  
  ### return result ----
  result
}

### Predict method for LDA objects
predict.lda_topic_model <- function(object, newdata, method = c("gibbs", "dot"), 
                                    iterations = NULL, burnin = -1, seed = NULL, 
                                    verbose = FALSE, ...) {
  
  ### Check inputs ----
  if (method[1] == "gibbs") {
    
    if (is.null(iterations)) {
      stop("when using method 'gibs' iterations must be specified.")
    }
    
    if (burnin >= iterations) {
      stop("burnin must be less than iterations")
    }
    
  }
  
  
  if (class(object) != "lda_topic_model") {
    stop("object must be a topic model object of class lda_topic_model")
  }
  
  if (sum(c("dgCMatrix", "character", "numeric") %in% class(newdata)) < 1) {
    stop("newdata must be a matrix of class dgCMatrix or a character vector")
  }
  
  if (class(newdata) == "numeric") { # if newdata is a numeric vector, assumed to be 1 document
    if (is.null(names(newdata))) {
      stop("it looks like newdata is a numeric vector without names. 
           Did you mean to pass a single document?
           If so, it needs a names attribute to index tokens")
    }
    
    new_names <- names(newdata)
    
    newdata <- Matrix::Matrix(newdata, nrow = 1)
    
    colnames(newdata) <- new_names
    
    rownames(newdata) <- 1
    
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

  if ("dgCMatrix" %in% class(newdata)) {
    dtm_newdata <- newdata
  } else {
    ## Add a check here to make sure it's the right object & return helpful error message
    
    data_args <- attr(object$data, "args")
    
    data_args$doc_vec <- newdata
    
    data_args$verbose <- verbose
    
    if (attr(object$data, "call") == "CreateDtm") {
      dtm_newdata <- do.call(CreateDtm, data_args)
    } else if (attr(object$data, "call") == "CreateTcm") {
      dtm_newdata <- do.call(CreateTcm, data_args)
    } else {
      stop("Something is wrong with object$data. Cannot find attribute 'call'.")
    }
  }
  
  ### Align vocabulary ----
  # this is fancy because of how we do indexing in gibbs sampling
  vocab_original <- colnames(object$data) # tokens in training set
  
  vocab_intersect <- intersect(vocab_original, colnames(dtm_newdata))
  
  vocab_add <- setdiff(vocab_original, vocab_intersect)
  
  add_mat <- Matrix::Matrix(0, nrow = nrow(dtm_newdata), ncol = length(vocab_add))
  
  colnames(add_mat) <- vocab_add
  
  dtm_newdata <- Matrix::cbind2(dtm_newdata, add_mat)
  
  if (nrow(dtm_newdata) == 1) {
    dtm_newdata <- Matrix::Matrix(dtm_newdata[,vocab_original], nrow = 1)
    
    colnames(dtm_newdata) <- vocab_original
    
    rownames(dtm_newdata) <- 1
  } else {
    
    dtm_newdata <- dtm_newdata[, vocab_original]
    
  }

  ### Get predictions ----
  
  if (method[1] == "dot") { # dot product method
    
    result <- dtm_newdata
    result <- (result / Matrix::rowSums(result)) %*% t(object$gamma[ ,vocab_original])
    result <- as.matrix(result)
    
  } else { # gibbs method
    # format inputs
    docs <- Dtm2Lexicon(dtm_newdata)
    
    Nd <- nrow(dtm_newdata)
    
    Nk <- nrow(object$phi)
    
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
