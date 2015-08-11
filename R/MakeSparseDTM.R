# Convert a sparse simple triplet document term matrix (tm package) to a sparse 
# Matrix object (Matrix package).
# This function is undocumented/internal to the package and not designed to be
# called by users.

MakeSparseDTM <- function(dtm){
  # dtm is a simple triplet matrix
  dtm.sparse <- sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, 
                             dims=c(dtm$nrow, dtm$ncol))
  
  rownames(dtm.sparse) <- Docs(dtm)
  colnames(dtm.sparse) <- Terms(dtm)
  
  return(dtm.sparse)
}
