#' Format Raw Output from lda::lda.collapsed.gibbs.sampler() 
#' @description extracts outputs from LDA model estimated with \code{lda} package by Jonathan Chang
#' @param lda.result The list value returned by lda.collapsed.gibbs.sampler()
#' @param docnames A character vector giving the names of documents. This is generally rownames(dtm). 
#' @param smooth Logical. Do you want to smooth your topic proportions so that 
#' there is a positive value for each term in each topic? Defaults to TRUE
#' @return
#' Returns a \code{list} with two elements: \code{phi} whose rows represent the 
#' distribution of words across a topic and \code{theta} whose rows represent 
#' the distribution of topics across a document. 
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm) 
#' 
#' # re-create a character vector of documents from the DTM
#' lex <- Dtm2Docs(nih_sample_dtm)
#' 
#' # Format for input to lda::lda.collapsed.gibbs.sampler
#' lex <- lda::lexicalize(lex, vocab=colnames(nih_sample_dtm))
#' 
#' # Fit the model from lda::lda.collapsed.gibbs.sampler
#' lda <- lda::lda.collapsed.gibbs.sampler(documents = lex, K = 100, 
#'                                          vocab = colnames(nih_sample_dtm), 
#'                                          num.iterations=200, 
#'                                          alpha=0.1, eta=0.05)
#'                                          
#' # Format the result to get phi and theta matrices                                        
#' lda <- FormatRawLdaOutput(lda.result=lda, docnames=rownames(nih_sample_dtm), smooth=TRUE)
#' 


FormatRawLdaOutput <- function(lda.result, docnames, smooth=TRUE){
    
    theta <- t(lda.result$document_sums)
    
  
  # Normalize topic vectors and doc vectors, smooth if necessary
    if(smooth){ 
      theta <- theta + 0.0001 
    }
	theta <- theta/Matrix::rowSums(theta)
	rownames(theta) <- docnames
	colnames(theta) <- paste("t_", 1:ncol(theta), sep="" )
  
  

	phi <- lda.result$topics
  
	if(smooth){ 
        phi <- phi + 0.0001 
	}
  
	phi <- phi/Matrix::rowSums(phi)
	rownames(phi) <- colnames(theta)

  # pull theta and phi into the result
	result <- list(theta=theta, phi=phi)
  
  # capture document_expects, if it exists 
	# (document_expects is over multiple runs, document_sums is over a single run)
  if(! is.null(dim(lda.result$document_expects))){
    theta_expects <- t(lda.result$document_expects)
    
    theta_expects <- theta_expects/Matrix::rowSums(theta_expects)
    rownames(theta_expects) <- docnames
    colnames(theta_expects) <- paste("t.", 1:ncol(theta_expects), sep="" )
    
    if(smooth){ 
      theta_expects <- theta_expects + 0.0001 
    }
    
    
    result$theta_expects <- theta_expects
    
  }
  

  # add in other outputs that may be in the raw lda.result
  additional_objects <- setdiff(names(lda.result), 
                                c("document_sums", "topics", "topic_sums", 
                                  "document_expects", "assignments"))
  
  additional_objects <- additional_objects[ ! is.na(additional_objects) ]
  
	if( length(additional_objects) > 0 ){ 
        result$etc <- lda.result[ additional_objects ]
	}

  # return result
	return(result)
}
