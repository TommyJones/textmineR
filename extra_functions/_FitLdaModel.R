#' Fit a topic model using Latent Dirichlet Allocation
#' @description A wrapper for two implementations of Latent Dirichlet 
#' Allocation that returns a nicely-formatted topic model. See details, below.
#' @param dtm A document term matrix of class \code{dgCMatrix}
#' @param k Number of topics
#' @param iterations The number of Gibbs iterations if \code{method = 'gibbs'}
#' @param alpha Dirichlet parameter for the distribution of topics over documents. 
#' Defaults to 0.1
#' @param beta Dirichlet parameter for the distribution of words over topics. 
#' Defaults to 0.05
#' @param smooth Logical indicating whether or not you want to smooth the 
#' probabilities in the rows of \code{phi} and \code{theta}.
#' @param method One of either 'gibbs' or 'vem' for either Gibbs sampling or 
#' variational expectation maximization. Defaults to 'gibbs'. See details, below.
#' @param return_all This argument is deprecated and will be removed.
#' @param ... Other arguments to pass to underlying functions. See details, below. 
#' @return Returns a list with a minumum of two objects, \code{phi} and 
#' \code{theta}. The rows of \code{phi} index topics and the columns index tokens.
#' The rows of \code{theta} index documents and the columns index topics.
#' @details 
#' For \code{method = 'gibbs'} this is a wrapper for \code{\link[text2vec]{LatentDirichletAllocation}} 
#' from the \code{text2vec} package. Additional arguments can be passed to
#'  \code{\link[text2vec]{LatentDirichletAllocation}} through \code{...}. However, there are some
#' arguments that, if passed through \code{...}, can cause conflicts. The 
#' arguments \code{n_topics}, \code{doc_topic_prior}, and \code{topic_word_prior} for 
#'  \code{\link[text2vec]{LatentDirichletAllocation}} are set with the arguments \code{k}, 
#' \code{alpha}, and \code{beta}, respectively.
#' 
#' For \code{method = 'vem'}, this function is a wrapper for \code{LDA} from the
#' \code{topicmodels} library. Arguments to \code{\link[topicmodels]{LDA}}'s \code{control}
#' argument are passed through \code{...}. \code{\link[topicmodels]{LDA}}, by default, has behavior
#' worth noting. By default, it estimates \code{alpha} and \code{beta} as part
#' of the expectation maximization. Therefore, the values of \code{alpha} and 
#' \code{beta} passed to \code{\link[topicmodels]{LDA}} will change unless \code{estimate.alpha} and
#' \code{estimate.beta} are passed to \code{...} and set to \code{FALSE}.
#' 
#' The \code{...} argument can also be used to control the underlying behavior of
#' \code{\link[textmineR]{TmParallelApply}}, such as the number of cpus, for example.
#' @examples
#' # Load a pre-formatted dtm 
#' data(nih_sample_dtm) 
#' 
#' # Fit an LDA model on a sample of documents
#' model <- FitLdaModel(dtm = nih_sample_dtm[ sample(1:nrow(nih_sample_dtm), 20), ], 
#'                      k = 5, iterations = 200)
#' 
#' str(model)
#' 
#' # Fit a model, include likelihoods passed to lda::lda.collapsed.gibbs.sampler
#' model <- FitLdaModel(dtm = nih_sample_dtm[ sample(1:nrow(nih_sample_dtm), 20), ], 
#'                      k = 5, iterations = 200, compute.log.likelihood = TRUE)
#' 
#' str(model)
#' @export
FitLdaModel <- function(dtm, k, iterations = NULL, alpha = 0.1, beta = 0.05, 
                        smooth = TRUE, method = "gibbs", return_all = NULL, ...){
  
  ### Perform some checks ------------------------------------------------------
  if (! is.null(return_all) ){
    message("The return_all argument is deprecated. It will be removed soon.")
  } 
  
  if(! method %in% c("gibbs", "vem") ){
    stop("Method must be one of 'gibbs' or 'vem'")
  }
  
  ### Some setup to match arguments given with dots ----------------------------
  dots <- list(...)

  ### Gibbs sampling method from text2vec package ------------------------------
  if(method == "gibbs"){
    if(is.null(iterations)){
      stop("'iterations' must be specified for method 'gibbs'")
    }
    
    vocab <- textmineR::GetVocabFromDtm(dtm = dtm)
    
    dtm_c <- text2vec::as.lda_c(dtm)
    
    lda_model <- text2vec::LatentDirichletAllocation$new(n_topics = k, vocabulary = vocab,
                                         doc_topic_prior = alpha,
                                         topic_word_prior = beta
                                         ) # dots[ names(dots) %in% names(formals(text2vec::LatentDirichletAllocation)) ]
    
    theta <- lda_model$fit_transform(dtm, n_iter = iterations, 
                                     check_convergence_every_n = max(5, round(iterations / 50)))
    
    phi <- lda_model$get_word_vectors()
    
    phi <- t(phi)
    
    if (smooth) {
      phi <- phi + min(1 / ncol(phi), 0.0001)
      theta <- theta + min(1 / ncol(theta) , 0.001)
    }
    
    theta <- theta / rowSums(theta)
    
    theta[ is.na(theta) ] <- 0
    
    phi <- phi / rowSums(phi)
    
    phi[ is.na(phi) ] <- 0
    
    colnames(theta) <- paste0("t_", 1:ncol(theta))
    rownames(phi) <- paste0("t_", 1:nrow(phi))
    
    result <- list(theta = theta,
                   phi = phi)
    
  } else {
    
    # below is an inelegant matching of control parameters to topicmodels::LDA
    ldacontrol <- c("seed", "verbose", "save", "prefix",
                    "nstart", "best", "keep", "estimate.beta",
                    "var", "em", "initialize", "alpha", "estimate.alpha")
    
    control <- dots[ names(dots) %in% ldacontrol ]
    
    if(! "alpha" %in% names(control)){
      control <- c(control, alpha = alpha)
    }
    
    model <- topicmodels::LDA(x = dtm, k = k, method = "VEM", 
                              control = control)
    
    theta <- model@gamma
    
    rownames(theta) <- model@documents
    colnames(theta) <- paste("t", 1:ncol(theta), sep = "_")
    
    phi <- exp(model@beta)
    colnames(phi) <- model@terms
    rownames(phi) <- colnames(theta)
    
    result <- list(theta = theta, phi = phi)
    
  }
  
  return(result)
}
  