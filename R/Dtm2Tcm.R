#' @title Turn a document term matrix into a term co-occurence matrix
#' @description Turn a document term matrix, whose rows index documents and 
#' whose columns index terms, into a term co-occurence matrix. A term co-occurence
#' matrix's rows and columns both index terms. See \code{details}, below.
#' @param dtm A document term matrix, generally of class \code{dgCMatrix}, though
#' other classes, such as \code{dgTMatrix}, may also work without issue.
#' @return Returns a square \code{dgCMatrix} whose rows and columns both index
#' terms. The i, j entries of this matrix represent the count of term j across
#' documents containing term i. Note that, while square, this matrix is not
#' symmetric.
#' @examples
#' data(nih_sample_dtm)
#' 
#' tcm <- Dtm2Tcm(nih_sample_dtm)
#' @export
Dtm2Tcm <- function(dtm){
  
  # create a binary matrix
  dtm_binary <- dtm > 0
  
  # dot product gives us the result
  result <- Matrix::t(dtm_binary) %*% dtm
  
  result
}
