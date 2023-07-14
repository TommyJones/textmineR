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
#' useful for quantifying uncertainty around parametersof the final model.
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
    par <- object$theta * (sum(object$alpha) + rowSums(object$data))
    
  } else if (which == "phi") {
    
    # need to recover approximate theta count mat to get number of times
    # each topic was sampled
    theta_count <- object$theta * (sum(object$alpha) + rowSums(object$data)) 
    theta_count <- t(theta_count) - object$alpha 
    theta_count <- t(theta_count) %>% round %>% colSums()
    
    # now get the right parameter matrix
    par <- object$phi * (sum(object$beta) + theta_count)
    
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
#'   See note below.
#' @return Returns a list with a minimum of two objects, \code{phi} and 
#' \code{theta}. The rows of \code{phi} index topics and the columns index tokens.
#' The rows of \code{theta} index documents and the columns index topics.
#' @note 
#' When passing additional arguments to \link[topicmodels]{CTM}, you must unlist the 
#' elements in the \code{control} argument and pass them one by one. See examples for
#' how to dot this correctly.
#' @examples
#' # Load a pre-formatted dtm 
#' data(nih_sample_dtm) 
#' 
#' # Fit a CTM model on a sample of documents
#' model <- FitCtmModel(dtm = nih_sample_dtm[ sample(1:nrow(nih_sample_dtm) , 10) , ], 
#'                      k = 3, return_all = FALSE)
#'                      
#' # the correct way to pass control arguments to CTM
#' \dontrun{
#' topics_CTM <- FitCtmModel(
#'     dtm = nih_sample_dtm[ sample(1:nrow(nih_sample_dtm) , 10) , ],
#'     k = 10,
#'     calc_coherence = TRUE,
#'     calc_r2 = TRUE,
#'     return_all = TRUE,
#'     estimate.beta = TRUE,
#'     verbose = 0,
#'     prefix = tempfile(),
#'     save = 0,
#'     keep = 0,
#'     seed = as.integer(Sys.time()),
#'     nstart = 1L,
#'     best = TRUE,
#'     var = list(iter.max = 500, tol = 10^-6),
#'     em = list(iter.max = 1000, tol = 10^-4),
#'     initialize = "random",
#'     cg = list(iter.max = 500, tol = 10^-5)
#' )
#' }
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
FitLsaModel <- function(dtm, k, calc_coherence = TRUE, return_all = FALSE, ...){
  
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
  
  # check inputs
  if (class(dtm) != "dgCMatrix" & class(dtm) != "numeric") {
    stop("dtm must be of class dgCMatrix or, if a single document, a numeric vector")
  }
  
  if (class(dtm) == "numeric") {
    
    if (is.null(names(dtm))) {
      stop("it looks like you passed a numeric vector without names. 
           Did you mean to pass a single document?
           If so, it needs a names attribute to index tokens")
      
    }
    
    vocab <- names(dtm)
    
    dtm <- Matrix::Matrix(dtm, nrow = 1, sparse = TRUE)
    
    colnames(dtm) <- vocab
    
  }
  
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
#' @param iterations Integer number of iterations for the Gibbs sampler to run. A
#'        future version may include automatic stopping criteria.
#' @param burnin Integer number of burnin iterations. If \code{burnin} is greater than -1,
#'        the resulting "phi" and "theta" matrices are an average over all iterations
#'        greater than \code{burnin}.
#' @param alpha Vector of length \code{k} for asymmetric or a number for symmetric.
#'        This is the prior for topics over documents
#' @param beta Vector of length \code{ncol(dtm)} for asymmetric or a number for symmetric.
#'        This is the prior for words over topics.
#' @param optimize_alpha Logical. Do you want to optimize alpha every 10 Gibbs iterations?
#'        Defaults to \code{FALSE}.
#' @param calc_likelihood Do you want to calculate the likelihood every 10 Gibbs iterations?
#'        Useful for assessing convergence. Defaults to \code{FALSE}. 
#' @param calc_coherence Do you want to calculate probabilistic coherence of topics
#'        after the model is trained? Defaults to \code{TRUE}. 
#' @param calc_r2 Do you want to calculate R-squared after the model is trained?
#'        Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}
#' @return Returns an S3 object of class c("LDA", "TopicModel"). DESCRIBE MORE
#' @details EXPLAIN IMPLEMENTATION DETAILS
#' @examples 
#' # load some data
#' data(nih_sample_dtm)
#' 
#' # fit a model 
#' set.seed(12345)
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
#' @export
FitLdaModel <- function(dtm, k, iterations = NULL, burnin = -1, alpha = 0.1, beta = 0.05, 
                        optimize_alpha = FALSE, calc_likelihood = FALSE, 
                        calc_coherence = TRUE, calc_r2 = FALSE, ...){
  
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
  
  if (! is.logical(calc_coherence))
    stop("calc_coherence must be logical")
  
  if (! is.logical(calc_r2))
    stop("calc_r2 must be logical")
  
  
  ### Format inputs ----
  
  # make beta a matrix to format for C funciton
  
  beta <- t(beta + matrix(0, nrow = length(beta), ncol = k))
  
  # other formatting
  docs <- Dtm2Lexicon(dtm,...)
  
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
  
  phi <- phi + beta # t(t(phi) + beta)
  
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
  
  names(result$alpha) <- rownames(phi)
  
  colnames(result$beta) <- colnames(phi) # because this is still a matrix
  
  result <- list(phi = phi, 
                 theta = theta, 
                 gamma = gamma,
                 data = dtm, 
                 alpha = result$alpha, 
                 beta = result$beta[1, ], # make beta a vector again
                 log_likelihood = data.frame(result$log_likelihood)[,1:2] # drop 3rd col for now
                 ) # add other things here
  
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
      stop("when using method 'gibs' iterations must be specified.")
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
  vocab_original <- colnames(object$data) # tokens in training set
  
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
    
    result <- dtm_newdata[,vocab_original]
    
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


#' Update a Latent Dirichlet Allocation topic model with new data
#' @description Update an LDA model with new data using collapsed Gibbs sampling. 
#' @param object a fitted object of class \code{lda_topic_model}
#' @param dtm A document term matrix or term co-occurrence matrix of class dgCMatrix.
#' @param additional_k Integer number of topics to add, defaults to 0.
#' @param iterations Integer number of iterations for the Gibbs sampler to run. A
#'        future version may include automatic stopping criteria.
#' @param burnin Integer number of burnin iterations. If \code{burnin} is greater than -1,
#'        the resulting "phi" and "theta" matrices are an average over all iterations
#'        greater than \code{burnin}.
#' @param new_alpha For now not used. This is the prior for topics over documents
#'        used when updating the model
#' @param new_beta For now not used. This is the prior for words over topics
#'        used when updating the model.
#' @param optimize_alpha Logical. Do you want to optimize alpha every 10 Gibbs iterations?
#'        Defaults to \code{FALSE}.
#' @param calc_likelihood Do you want to calculate the likelihood every 10 Gibbs iterations?
#'        Useful for assessing convergence. Defaults to \code{FALSE}. 
#' @param calc_coherence Do you want to calculate probabilistic coherence of topics
#'        after the model is trained? Defaults to \code{TRUE}. 
#' @param calc_r2 Do you want to calculate R-squared after the model is trained?
#'        Defaults to \code{FALSE}.
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}
#' @return Returns an S3 object of class c("LDA", "TopicModel"). 
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
                                   iterations = NULL, burnin = -1, 
                                   new_alpha = NULL, new_beta = NULL, 
                                   optimize_alpha = FALSE, calc_likelihood = FALSE, 
                                   calc_coherence = TRUE, calc_r2 = FALSE, ...) {
  
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
    message("dtm is not of class dgCMatrix, attempting to convert...")
    
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
  
  
  ##############################################################################
  # Check dimensions of additional alpha and beta
  # Need solution of how to not use defaults first
  ##############################################################################
  
  if (! is.null(new_alpha) | ! is.null(new_beta)) {
    warning("methods for new_alpha and new_beta are not yet implemented. 
            See 'details' under help(update.lda_topic_model) for more information.")
  }
  
  # alpha and beta?
  # if (! is.numeric(alpha) | sum(is.na(alpha)) > 0)
  #   stop("alpha must be a numeric scalar or vector with no missing values")
  # 
  # if (length(alpha) == 1) {
  #   alpha <- numeric(k) + alpha
  # } else if (length(alpha) != k){
  #   stop("alpha must be a scalar or vector of length k")
  # }
  # 
  # if (! is.numeric(beta) | sum(is.na(beta)) > 0)
  #   stop("beta must be a numeric scalar or vector with no missing values")
  # 
  # if (length(beta) == 1) {
  #   beta <- numeric(ncol(dtm)) + beta
  # } else if (length(beta) != ncol(dtm)){
  #   stop("beta must be a scalar or vector of length ncol(dtm)")
  # }
  
  if (! is.logical(calc_coherence))
    stop("calc_coherence must be logical")
  
  if (! is.logical(calc_r2))
    stop("calc_r2 must be logical")
  
  ### Format inputs ----
  
  
  # beta prior inherets from object$phi
  
  # align vocab in intelligent way for adding new vocab
  beta <- object$phi
  
  v_diff <- setdiff(colnames(dtm), colnames(object$phi))
  
  m_add <- matrix(0, nrow = nrow(object$phi), ncol = length(v_diff))
  
  colnames(m_add) <- v_diff
  
  m_add <- m_add + mean(object$phi) # uniform prior over new words
  
  beta <- cbind(beta, m_add)
  
  beta <- beta[,colnames(dtm)]
  
  # add topics 
  m_add <- matrix(0, 
                  nrow = additional_k, 
                  ncol = ncol(beta))
  
  m_add <- m_add + mean(object$phi) # uniform prior for all words in new topics
  
  beta <- rbind(beta, m_add)
  
  # magnitude of rows of beta prior, also hack MUST FIX
  beta <- beta / rowSums(beta) # normalize rows
  
  beta <- (mean(object$beta) * ncol(beta)) * beta
  
  # alpha prior inherets from object$alpha
  alpha <- c(object$alpha, rep(mean(object$alpha), additional_k)) # rep(0.1, Nk)
  
  # additional constants for fitting
  docs <- Dtm2Lexicon(dtm,...)
  
  Nd <- nrow(dtm)
  
  Nk <- nrow(beta)
  
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
  
  phi <- phi + beta
  
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
  
  names(result$alpha) <- rownames(result$phi)
  
  names(result$beta) <- colnames(result$phi)
  
  result <- list(phi = phi, theta = theta, gamma = gamma,
                 data = dtm, alpha = result$alpha, beta = result$beta,
                 log_likelihood = data.frame(result$log_likelihood)[,1:2] # drop 3rd col for now
  ) # add other things here
  
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
