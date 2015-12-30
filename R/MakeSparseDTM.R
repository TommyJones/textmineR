#' Convert a sparse simple triplet document term matrix to a sparse Matrix
#' 
#' @description This function takes a \code{DocumentTermMatrix} from the 
#' \code{tm} package, and converts it to a \code{CSparse} matrix from the 
#' \code{Matrix} package. Generally, it is designed to be an internal function
#' to \code{textmineR}. However, it is available in case users want to use the 
#' \code{tm} package directly to make a DTM.
#' @param dtm A \code{DocumentTermMatrix} object from the \code{tm} package
#' @return
#' Returns an object of class \code{dgCMatrix}, a sparse matrix, whose columns
#' correspond to terms and rows correspond to documents.
#' @export
#' @examples
#' \dontrun{
#' my_dtm <- tm::DocumentTermMatrix(my_corpus_object)
#' 
#' MakeSparseDtm(dtm = my_dtm)
#' }


MakeSparseDTM <- function(dtm){
  # dtm is a simple triplet matrix
  dtm.sparse <- Matrix::sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, 
                                     dims=c(dtm$nrow, dtm$ncol))
  
  rownames(dtm.sparse) <- tm::Docs(dtm)
  colnames(dtm.sparse) <- tm::Terms(dtm)
  
  return(dtm.sparse)
}
