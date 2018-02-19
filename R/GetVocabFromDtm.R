#' Reconstruct a \code{text2vec::vocabulary} object from a document term matrix.
#' @description Reconstructs a \code{text2vec::vocabulary} object from a document 
#' term matrix. See \code{\link[text2vec]{create_vocabulary}} for more details.
#' @param dtm A document term matrix of class \code{dgCMatrix}
#' @return Returns a \code{text2vec::vocabulary} object. 
#' See \code{\link[text2vec]{create_vocabulary}} for more details.
#' @examples
#' # Load a pre-formatted dtm 
#' data(nih_sample_dtm) 
#' 
#' vocab <- GetVocabFromDtm(dtm = nih_sample_dtm)
#' 
#' str(vocab)
#' @export
GetVocabFromDtm <- function(dtm){ 
  
  vocab <- list(
    terms = colnames(dtm),
    terms_counts = colSums(dtm),
    doc_counts = colSums(dtm > 0)
  )
  
  ngram <- sapply(strsplit(vocab$terms, split = "_"), length)
  
  ngram <- c(min(ngram), max(ngram))
  
  document_count <- nrow(dtm)
  
  stopwords <- vector(mode = "character", length = 0)
  
  sep_ngram <- "_"
  
  result <- list(vocab = vocab,
                 ngram = ngram,
                 document_count = document_count,
                 stopwords = stopwords,
                 sep_ngram = sep_ngram)
  
  class(result) <- "text2vec_vocabulary"
  
  result
}
