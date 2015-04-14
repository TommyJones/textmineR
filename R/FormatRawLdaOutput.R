#' Format Raw Output from lda.collapsed.gibbs.sampler() 
#' @description extracts outputs from LDA model estimated with "lda" package by Jonathan Chang
#' @param lda.result The list value returned by lda.collapsed.gibbs.sampler()
#' @param docnames A character vector giving the names of documents. This is generally rownames(dtm). 
#' @param smooth Logical. Do you want to smooth your topic proportions so that there is a positive value for each term in each topic? Defaults to TRUE
#' @export
#' @examples
#' lex <- ConvertDtm2Docs(dtm)
#' lex <- lexicalize(lex, vocab=colnames(dtm))
#' lda <- lda.collapsed.gibbs.sampler(documents = lex, K = 100, vocab = colnames(dtm), num.iterations=2000, alpha=0.1, eta=0.05)
#' lda <- FormatRawLdaOutput(lda.result=lda, docnames=rownames(dtm), smooth=TRUE)


FormatRawLdaOutput <- function(lda.result, docnames, smooth=TRUE){
    
    theta <- t(lda.result$document_sums)
    
  
  # Normalize topic vectors and doc vectors, smooth if necessary
    if(smooth){ 
      theta <- theta + 0.0001 
    }
	theta <- theta/Matrix::rowSums(theta)
	rownames(theta) <- docnames
	colnames(theta) <- paste("t.", 1:ncol(theta), sep="" )
  
  

	phi <- lda.result$topics
  
	if(smooth){ 
        phi <- phi + 0.0001 
	}
  
	phi <- phi/Matrix::rowSums(phi)
	rownames(phi) <- colnames(theta)

  # pull theta and phi into the result
	result <- list(theta=theta, phi=phi)
  
  # capture document_expects, if it exists (document_expects is over multiple runs, document_sums is over a single run)
  if(! is.null(dim(lda.result$document_expects))){
    theta.expects <- t(lda.result$document_expects)
    
    theta.expects <- theta.expects/Matrix::rowSums(theta.expects)
    rownames(theta.expects) <- docnames
    colnames(theta.expects) <- paste("t.", 1:ncol(theta.expects), sep="" )
    
    if(smooth){ 
      theta.expects <- theta.expects + 0.0001 
    }
    
    
    result$theta.expects <- theta.expects
    
  }
  

  # add in likelihoods if necessary
	if( "log.likelihoods" %in% names(lda.result) ){ 
        result$likelihood <- lda.result$log.likelihoods
	}

  # return result
	return(result)
}
