#' Convert a DTM to a Character Vector of documents
#' 
#' @description This function takes a sparse matrix (DTM) as input and returns a character vector
#' whose length is equal to the number of rows of the input DTM.
#' @param dtm A sparse Matrix from the matrix package whose rownames correspond 
#' to documents and colnames correspond to words
#' @param ... Other arguments to be passed to \code{TmParallelApply}. See note, below.
#' @return
#' Returns a character vector. Each entry of this vector corresponds to the rows
#' of \code{dtm}.
#' @note
#' This function performs parallel computation if \code{dtm} has more than 3,000
#' rows. The default is to use all available cores according to \code{parallel::detectCores()}.
#' However, this can be modified by passing the \code{cpus} argument when calling
#' this function.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample)
#' data(nih_sample_dtm) 
#' 
#' # see the original documents
#' nih_sample$ABSTRACT_TEXT[ 1:3 ]
#' 
#' # see the new documents re-structured from the DTM
#' new_docs <- Dtm2Docs(dtm = nih_sample_dtm)
#' 
#' new_docs[ 1:3 ]
#' 
Dtm2Docs <- function(dtm, ...){
  
  # do in parallel in batches of about 3000 if we have more than 3000 docs
  if(nrow(dtm) > 3000){
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    dtm_list <- lapply(batches, function(x) dtm[ x:min(x + 2999, nrow(dtm)) , ])
    
    out <-TmParallelApply(X = dtm_list, FUN = function(x){
      Dtm2DocsC(dtm = x, vocab = colnames(x))
    }, ...)
    
  }else{
    out <- Dtm2DocsC(dtm = dtm, vocab = colnames(dtm))
  }
  
  out <- unlist(out)
  
  names(out) <- rownames(dtm)
  
  out
}

