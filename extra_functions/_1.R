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
  
  # first element of type meets expectations
  if (! type[1] %in% c("weights", "prior", "both"))
    stop("type must be one of 'weights', 'prior', or 'both'")
  
  
  # calc_coherence meets expectations?
  if (! is.logical(calc_coherence))
    stop("calc_coherence must be logical")
  
  # calc_r2 meets expectations?
  if (! is.logical(calc_r2))
    stop("calc_r2 must be logical")
  
  ### Format inputs ----
  
  # prepare the weights
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
  
  # align vocabulary for extra topics (if any)
  
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


