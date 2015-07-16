#' Get some topic labels using a "more probable" method of terms
#' 
#' @description Function calls \code{textmineR::GetProbableTerms()} with some rules to get topic labels. This function is in "super-ultra-mega alpha". 
#' @param assignments = a documents X topics matrix similar to \code{theta}. This will work best if this matrix is sparse, with only a few non-zero topics per document.
#' @param dtm = a document term matrix, preferably a Matrix package sparse matrix
#' @param M = the number of n-gram labels you want to return. Defaults to 2
#' @export
#' @examples
#' assignments <- t(apply(theta, 1, function(x){
#'   x[ x < 0.05 ] <- 0
#'   x / sum(x)
#' }))
#' 
#' labels <- LabelTopics(assignments = assignments, dtm = dtm, M = 2)

LabelTopics <- function(assignments, dtm, M=2){
  # figure out a threshold
  threshold <- min(apply(assignments, 2, max))
  
  # get a list of documents for each topic
  doc_list <- apply(assignments, 2, function(x){
    names(x)[ x >= threshold ]
  })
  
  # get dtm_ngram and p_terms
  dtm_ngram <- dtm[ , grepl("_", colnames(dtm)) ]
  p_terms <- Matrix::colSums(dtm_ngram)
  p_terms <- p_terms / sum(p_terms)
  
  # apply the label algorithm over each topic
  result <- lapply(doc_list, function(x){
    l <- GetProbableTerms(docnames = x, dtm = dtm_ngram, p.terms = p_terms)
    names(l)[ order(l, decreasing=T) ][ 1:M ]
  })
  result
}