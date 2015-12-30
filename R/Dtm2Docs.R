#' Convert a DTM to a Character Vector of documents
#' 
#' @description This function takes a sparse matrix (DTM) as input and returns a character vector
#' whose length is equal to the number of rows of the input DTM.
#' @param dtm A sparse Matrix from the matrix package whose rownames correspond 
#' to documents and colnames correspond to words
#' @return
#' Returns a character vector. Each entry of this vector corresponds to the rows
#' of \code{dtm}.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(acq2) 
#' 
#' # see the original documents
#' documents[ 1:3 ]
#' 
#' # see the new documents re-structured from the DTM
#' new_docs <- Dtm2Docs(dtm=dtm)
#' 
#' new_docs[ 1:3 ]
#' 
Dtm2Docs <- function(dtm){
  
  # do in parallel in batches of about 3000 if we have more than 3000 docs
  if(nrow(dtm) > 3000){
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    dtm_list <- lapply(batches, function(x) dtm[ x:min(x + 2999, nrow(dtm)) , ])
    
    out <-TmParallelApply(X = dtm_list, FUN = function(x){
      Dtm2DocsC(dtm = x, vocab = colnames(x))
    })
    
  }else{
    out <- Dtm2DocsC(dtm = dtm, vocab = colnames(dtm))
  }
  
  out <- unlist(out)
  
  names(out) <- rownames(dtm)
  
  out
}

