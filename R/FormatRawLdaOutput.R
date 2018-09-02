#' Format Raw Output from \code{\link[lda]{lda.collapsed.gibbs.sampler}} 
#' @description extracts outputs from LDA model estimated with \code{lda} package by Jonathan Chang
#' @param lda_result The list value returned by \code{\link[lda]{lda.collapsed.gibbs.sampler}}
#' @param docnames A character vector giving the names of documents. This is generally rownames(dtm). 
#' @param smooth Logical. Do you want to smooth your topic proportions so that 
#' there is a positive value for each term in each topic? Defaults to TRUE
#' @param softmax Logical. Do you want to use the softmax function to normalize
#' raw output? If FALSE (the default) output is normalized using standard sum.
#' @return
#' Returns a \code{list} with two elements: \code{phi} whose rows represent the 
#' distribution of words across a topic and \code{theta} whose rows represent 
#' the distribution of topics across a document. 
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm) 
#' 
#' # Get a sample of documents
#' dtm <- nih_sample_dtm[ sample(1:nrow(nih_sample_dtm), 20) , ]
#' 
#' # re-create a character vector of documents from the DTM
#' lex <- Dtm2Docs(dtm)
#' 
#' # Format for input to lda::lda.collapsed.gibbs.sampler
#' lex <- lda::lexicalize(lex, vocab=colnames(dtm))
#' 
#' # Fit the model from lda::lda.collapsed.gibbs.sampler
#' lda <- lda::lda.collapsed.gibbs.sampler(documents = lex, K = 5, 
#'                                          vocab = colnames(dtm), 
#'                                          num.iterations=200, 
#'                                          alpha=0.1, eta=0.05)
#'                                          
#' # Format the result to get phi and theta matrices                                        
#' lda <- FormatRawLdaOutput(lda_result=lda, docnames=rownames(dtm), smooth=TRUE)
#' 


FormatRawLdaOutput <- function(lda_result, docnames, smooth=TRUE, softmax = FALSE){
  .Deprecated(msg = "FormatRawLdaOutput is slated for deletion in textmineR v3.0. Please plan accordingly.
              Please submit questions or requests to 	https://github.com/TommyJones/textmineR/issues")
  
    theta <- t(lda_result$document_sums)
    
  
  # Normalize topic vectors and doc vectors, smooth if necessary
    if(smooth){ 
      theta <- theta + 0.0001 
    }
    
    if (softmax){
      theta <- exp(theta) / Matrix::rowSums(exp(theta))
    } else {
      theta <- theta / Matrix::rowSums(theta)
    }
	rownames(theta) <- docnames
	colnames(theta) <- paste("t_", 1:ncol(theta), sep="" )
  
  

	phi <- lda_result$topics
  
	if(smooth){ 
        phi <- phi + 0.0001 
	}
  
	if (softmax) {
	  phi <- exp(phi) / Matrix::rowSums(exp(phi))
	  
	} else {
	  phi <- phi / Matrix::rowSums(phi)
	  
	}
	rownames(phi) <- colnames(theta)

  # pull theta and phi into the result
	result <- list(theta=theta, phi=phi)
  
  # capture document_expects, if it exists 
	# (document_expects is over multiple runs, document_sums is over a single run)
  if(! is.null(dim(lda_result$document_expects))){
    theta_expects <- t(lda_result$document_expects)
    
    if(smooth){ 
      theta_expects <- theta_expects + 0.0001 
    }
    
    if(softmax){
      theta_expects <- exp(theta_expects) / Matrix::rowSums(exp(theta_expects))
      
    } else {
      theta_expects <- theta_expects / Matrix::rowSums(theta_expects)
      
    }
    
    rownames(theta_expects) <- docnames
    colnames(theta_expects) <- paste("t.", 1:ncol(theta_expects), sep="" )
    
    
    
    result$theta_expects <- theta_expects
    
  }
  

  # add in other outputs that may be in the raw lda_result
  additional_objects <- setdiff(names(lda_result), 
                                c("document_sums", "topics", "topic_sums", 
                                  "document_expects", "assignments"))
  
  additional_objects <- additional_objects[ ! is.na(additional_objects) ]
  
	if( length(additional_objects) > 0 ){ 
        result$etc <- lda_result[ additional_objects ]
	}

  # return result
	return(result)
}
