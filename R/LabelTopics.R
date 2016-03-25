#' Get some topic labels using a "more probable" method of terms
#' 
#' @description Function calls \code{textmineR::GetProbableTerms()} with some 
#' rules to get topic labels. This function is in "super-ultra-mega alpha"; use
#' at your own risk/discretion. 
#' @param assignments A documents by topics matrix similar to \code{theta}. 
#' This will work best if this matrix is sparse, with only a few non-zero topics 
#' per document.
#' @param dtm A document term matrix of class \code{matrix} or \code{dgCMatrix}.
#' The columns of \code{dtm} should be n-grams whose colnames have a "_" where
#' spaces would be between the words.
#' @param M The number of n-gram labels you want to return. Defaults to 2
#' @return Returns a \code{matrix} whose rows correspond to topics and whose
#' j-th column corresponds to the j-th "best" label assignment.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm)
#' data(nih_sample_topic_model) 
#' 
#' assignments <- t(apply(nih_sample_topic_model$theta, 1, function(x){
#'   x[ x < 0.05 ] <- 0
#'   x / sum(x)
#' }))
#' 
#' labels <- LabelTopics(assignments = assignments, dtm = nih_sample_dtm, M = 2)
#' 

LabelTopics <- function(assignments, dtm, M=2){
  # figure out a threshold
  threshold <- apply(assignments, 2, function(x) max(x, na.rm=T))
  threshold <- min(threshold[ threshold > 0 & ! is.na(threshold) ])
  
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
  
  # format into a matrix for output
  result <- do.call(rbind, result)
  
  colnames(result) <- paste("label_", 1:ncol(result), sep="")
  
  result
}