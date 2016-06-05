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
#' @param return_all Logical. Do you want the raw results of the underlying 
#' function returned along with the formatted results? Defaults to \code{TRUE}.
#' @param ... Other arguments to pass to underlying functions. See details, below. 
#' @return Returns a list with a minumum of two objects, \code{phi} and 
#' \code{theta}. The rows of \code{phi} index topics and the columns index tokens.
#' The rows of \code{theta} index documents and the columns index topics.
#' @details 
#' For \code{method = 'gibbs'} this is a wrapper for \code{\link[lda]{lda.collapsed.gibbs.sampler}} 
#' from the \code{lda} package. Additional arguments can be passed to
#'  \code{\link[lda]{lda.collapsed.gibbs.sampler}} through \code{...}. However, there are some
#' arguments that, if passed through \code{...}, can cause conflicts. The 
#' arguments \code{K}, \code{alpha}, and \code{eta} for 
#'  \code{\link[lda]{lda.collapsed.gibbs.sampler}} are set with the arguments \code{k}, 
#' \code{alpha}, and \code{beta}, respectively. The arguments \code{documents} 
#' and \code{vocab} for  \code{\link[lda]{lda.collapsed.gibbs.sampler}} are set by \code{dtm} 
#' and aren't required. 
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
                        smooth = TRUE, method = "gibbs", return_all = FALSE, ...){
  # Some setup to match arguments given with ...
  dots <- list(...)
  
  
  if(! method %in% c("gibbs", "vem") ){
    stop("Method must be one of 'gibbs' or 'vem'")
  }
  
  if(method == "gibbs"){
    if(is.null(iterations)){
      stop("'iterations' must be specified for method 'gibbs'")
  }

    
    
    
    vocab <- colnames(dtm)
    
    lex_args <- c(list(dtm = dtm), dots[ names(dots) %in% 
                                           names(formals(textmineR::TmParallelApply)) ])
    
    lex <- do.call(textmineR::Dtm2Docs, lex_args)
    
    if(nrow(dtm) > 1000){
      chunks <- seq(1, nrow(dtm), by = 1000)
      
      lex_list <- lapply(chunks, function(x) lex[ x:min(x + 999, length(lex)) ])
      
      lex_args <- c(list(X = lex_list, 
                         FUN = function(x){
                           lda::lexicalize(x, sep = " ", vocab = vocab)
                         },
                         export = "vocab"),
                         dots[ names(dots) %in% 
                                 names(formals(textmineR::TmParallelApply)) ])
      
      lex <- do.call(textmineR::TmParallelApply, lex_args)
      
      lex <- do.call(c, lex)
      
    }else{
      lex <- lda::lexicalize(lex, sep=" ", vocab=vocab)
    }
    
    model_args <- c(list(documents = lex, 
                         K = k, 
                         vocab = vocab, 
                         num.iterations = iterations, 
                         alpha = alpha, 
                         eta = beta),
                    dots[ names(dots) %in% 
                            names(formals(lda::lda.collapsed.gibbs.sampler)) ] )
    
    model <- do.call(lda::lda.collapsed.gibbs.sampler, model_args)
    
    result <- textmineR::FormatRawLdaOutput(lda_result = model, 
                                            docnames = rownames(dtm), 
                                            smooth = smooth)
    if(return_all){
      result <- c(result, etc = model)
    }
  }else{
    
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
    
    if(return_all){
      result <- c(result, etc = model)
    }
  }
  
  return(result)
  
}

