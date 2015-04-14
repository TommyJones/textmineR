#' @title Convert a sparse simple triplet document term matrix (tm package) to a sparse Matrix object (Matrix package).
#' 
#' @description This function converts objects of class DocumentTermMatrix to an object of class Matrix. Document
#' term matrices are assumed to be of class Matrix for the majority of text mining functions in package \code{textmineR}.
#'
#' @param dtm A document term matrix of class DocumentTermMatrix (tm package) 
#' @export
#' @examples
#' mydocs <- c("the quick brown fox", "the slow gray fox", "look at my horse", "my horse is amazing")
#' mycorp <- Corpus(VectorSource(mydocs))
#' mydtm <- DocumentTermMatrix(mycorp)
#' mydtm_sparse <- MakeSparseDTM(dtm=mydtm)

MakeSparseDTM <- function(dtm){
  # dtm is a simple triplet matrix
  dtm.sparse <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, 
                             dims=c(dtm$nrow, dtm$ncol))
  
  rownames(dtm.sparse) <- Docs(dtm)
  colnames(dtm.sparse) <- Terms(dtm)
  
  return(dtm.sparse)
}
