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
  
  # allocate some counts that are needed for c++
  n_d <- rowSums(dtm)
  
  theta_counts <- gtools::rdirichlet(n = Nd, alpha = alpha)
  
  theta_counts <- (theta_counts * n_d) %>% round()
  
  phi_counts <- gtools::rdirichlet(n = Nk, alpha = beta[1, ])
  
  phi_counts <- (phi_counts * colSums(theta_counts)) %>% round()
  
  n_z <- colSums(theta_counts)
  
  z_dn <- z_dn <- parallel::mcmapply(FUN = function(n, p)
    n = n_d, 
    p = lapply(1:nrow(theta_counts), function(j) theta_counts[j,]),
    SIMPLIFY = FALSE, 
    ...)
  
  ### run C++ gibbs sampler ----
  
  result <- fit_lda_c(docs = docs, Nk = Nk, Nd = Nd, Nv = Nv, 
                      alph = alpha, beta = beta,
                      theta_counts = theta_counts,
                      phi_counts = phi_counts,
                      n_d = n_d, n_z = n_z, z_dn = z_dn,
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

