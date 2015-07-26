#' Distance between all topics across many models, used for creating ensemble topic models
#' @description calculate the pairwise distance between topics in a list of topic models
#' Assumes that rownames of phi matrices are unique, meaning that
#' rownames also include information on which model the topic came from
#' 
#' @param posteriors A list where each element is itself a list containing two matrices, phi and theta.
#' @param method A character string describing the distance method to use. Only "cosine" is supported.
#' @export
#' @examples
#' lex <- ConvertDtm2Docs(dtm)
#' lex <- lexicalize(lex, vocab=colnames(dtm))
#' my_posteriors <- lapply(1:100, function(iteration){
#'   lda <- lda.collapsed.gibbs.sampler(documents = lex, K = 100, vocab = colnames(dtm), num.iterations=2000, alpha=0.1, eta=0.05)
#'   lda <- FormatRawLdaOutput(lda.result=lda, docnames=rownames(dtm), smooth=TRUE)
#' })
#' 
#' for(j in 1:length(myposteriors)){
#'   rownames(myposteriors[[ j ]]$theta) <- paste(rownames(myposteriors[[ j ]]$theta), j, sep="_")
#'   colnames(myposteriors[[ j ]]$phi) <- paste(colnames(myposteriors[[ j ]]$phi), j, sep="_")
#' }
#' 
#' myD <- EnsembleTopicDist(posteriors=myposteriors, method="cosine")

EnsembleTopicDist <- function(posteriors, method="cosine"){
	
  library(Matrix)
  
	# combine phi matrices into one gigantic matrix
  combined.topics <- lapply(posteriors, function(x) Matrix(x$phi, sparse=TRUE))
  
  combined.topics <- do.call(rBind, combined.topics)
	
	# calculate distance based on method
	if(method=="cosine"){
		Dfun <- function(input.matrix){
			# get unit length vectors (in parallel)
		  vec.length <- sqrt( Matrix::rowSums( input.matrix * input.matrix) )
			input.matrix <- input.matrix / vec.length
      
			# take dot product for cosine similarity
			input.matrix <- input.matrix %*% Matrix::t(input.matrix)
      
			# take 1 - cosine similarity for distance
			input.matrix <- 1 - input.matrix
      
			# return matrix
      return(input.matrix)
		}
	}else{ 
      stop("Only method='cosine' is supported in this version")
	}
    
  result <- Dfun(input.matrix = combined.topics)
  
  return(result)
}
