#' Fit a topic model using Latent Dirichlet Allocation
#' @description A wrapper for \code{lda::lda.collapsed.gibbs.sampler} that returns 
#' a nicely-formatted topic model.
#' @param dtm A document term matrix of class \code{Matrix::dgCMatrix}
#' @param k Number of topics
#' @param iterations The number of Gibbs iterations
#' @param alpha Dirichlet parameter for the distribution of topics over documents. 
#' Defaults to 0.1
#' @param beta Dirichlet parameter for the distribution of words over topics. 
#' Defaults to 0.05
#' @param smooth Logical indicating whether or not you want to smooth the 
#' probabilities in the rows of \code{phi} and \code{theta}.
#' @param ... Other arguments to pass to \code{lda::lda.collapsed.gibbs.sampler}. 
#' @return Returns a list with a minumum of two objects, \code{phi} and 
#' \code{theta}. The rows of \code{phi} index topics and the columns index tokens.
#' The rows of \code{theta} index documents and the columns index topics.
#' @export
#' @examples
#' # Load a pre-formatted dtm 
#' data(acq2) 
#' 
#' # Fit an LDA model
#' model <- FitLdaModel(dtm = dtm, k = 5, iterations = 500)
#' 
#' str(model)
#' 
#' # Fit a model, include likelihoods passed to lda::lda.collapsed.gibbs.sampler
#' model <- FitLdaModel(dtm = dtm, k = 5, iterations = 500, compute.log.likelihood = TRUE)
#' 
#' str(model)
#' 


FitLdaModel <- function(dtm, k, iterations, alpha = 0.1, beta = 0.05, 
                        smooth = T, ...){
  
  vocab <- colnames(dtm)
  
  lex <- Dtm2Docs(dtm = dtm)
  
  lex <- lda::lexicalize(lex, sep=" ", vocab=vocab)
  
  model <- lda::lda.collapsed.gibbs.sampler(documents = lex, 
                                            K = k, 
                                            vocab = vocab, 
                                            num.iterations = iterations, 
                                            alpha = alpha, 
                                            eta = beta,
                                            ...)
  
  model <- FormatRawLdaOutput(lda.result = model, 
                              docnames = rownames(dtm), 
                              smooth = smooth)
  
  return(model)
  
}

