#' Fit a topic model using Latent Dirichlet Allocation
#' @description A wrapper for \code{lda::lda.collapsed.gibbs.sampler} that returns nicely formatted objects.
#' @param dtm A document term matrix of class \code{Matrix::dgCMatrix}
#' @param k Number of topics
#' @param iterations The number of Gibbs iterations
#' @param alpha Dirichlet parameter for the distribution of topics over documents. Defaults to 0.1
#' @param beta Dirichlet parameter for the distribution of words over topics. Defaults to 0.05
#' @param ... Other arguments to pass to \code{lda::lda.collapsed.gibbs.sampler}. 
#' @export
#' @examples
#' # fit a model with the default options
#' model <- FitLdaModel(dtm = mydtm, iterations = 2000)
#' 
#' # include likelihoods
#' model <- FitLdaModel(dtm = mydtm, iterations = 2000, compute.log.likelihood = T)

FitLdaModel <- function(dtm, k, iterations, alpha = 0.1, beta = 0.05, ...){
  
  vocab <- colnames(dtm)
  
  lex <- Dtm2Docs(dtm = dtm)
  
  lex <- lexicalize(lex, sep=" ", vocab=vocab)
  
  model <- lda.collapsed.gibbs.sampler(documents = lex, 
                                       K = k, 
                                       vocab = vocab, 
                                       num.iterations = iterations, 
                                       alpha = alpha, 
                                       eta = beta,
                                       ...)
  
  model <- FormatRawLdaOutput(lda.result = model, 
                              docnames = rownames(dtm), 
                              smooth = T)
  
  return(model)
  
}

