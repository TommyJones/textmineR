#' Convert a DTM to a Character Vector of documents
#' 
#' @description This function takes a sparse matrix (DTM) as input and returns a character vector
#' whose length is equal to the number of rows of the input DTM.
#' @param dtm A sparse Matrix from the matrix package whose rownames correspond to documents and colnames correspond to words
#' @param parallel Do you want to parallelize this function using snowfall? Default is FALSE 
#' @param cpus If parallel is TRUE, the number of threads to use. (Recommendation is 4, for memory's sake)
#' @export
#' @examples
#' Dtm2Docs(dtm=mydtm)

Dtm2Docs <- function(dtm){
  
  out <- Dtm2DocsC(dtm = dtm, vocab = colnames(dtm))
  
  out <- unlist(out)
  
  names(out) <- rownames(dtm)
  
  out
}

