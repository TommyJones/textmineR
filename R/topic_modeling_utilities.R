### PUT A TOPIC MODEL SUMMARY FUNCTION HERE ----


#' Get cluster labels using a "more probable" method of terms
#' 
#' @description Function extracts probable terms from a set of documents. 
#' Probable here implies more probable than in a corpus overall.
#' @param docnames A character vector of rownames of dtm for set of documents
#' @param dtm A document term matrix of class \code{matrix} or \code{dgCMatrix}.
#' @param p_terms If not NULL (the default), a numeric vector representing the 
#' probability of each term in the corpus whose names correspond to colnames(dtm). 
#' @return
#' Returns a numeric vector of the format p_terms. The entries of the vectors 
#' correspond to the difference in the probability of drawing a term from the 
#' set of documents given by docnames and the probability of drawing that term 
#' from the corpus overall (p_terms).
#' 
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_topic_model)
#' data(nih_sample_dtm) 
#' 
#' # documents with a topic proportion of .25 or higher for topic 2
#' mydocs <- rownames(nih_sample_topic_model$theta)[ nih_sample_topic_model$theta[ , 2 ] >= 0.25 ] 
#' 
#' term_probs <- Matrix::colSums(nih_sample_dtm) / sum(Matrix::colSums(nih_sample_dtm))
#' 
#' GetProbableTerms(docnames = mydocs, dtm = nih_sample_dtm, p_terms = term_probs)
#' 
GetProbableTerms <- function(docnames, dtm, p_terms=NULL){
  
  
  # if p_terms is NULL, then create p_terms
  if( is.null(p_terms) ){
    p_terms <- Matrix::colSums(dtm) / sum(Matrix::colSums(dtm))
  }
  
  # get probability of terms given docnames
  if( length(docnames) == 1 ){ 
    p_terms.given.docs <- dtm[ docnames , ] 
  }else{
    p_terms.given.docs <- Matrix::colSums(dtm[ docnames , ])
  }
  
  p_terms.given.docs <- p_terms.given.docs/sum(p_terms.given.docs)
  
  # get our result, the difference
  result <- p_terms.given.docs - p_terms
  
  names(result) <- colnames(dtm)
  
  return(result)
  
}



#' Get some topic labels using a "more probable" method of terms
#' 
#' @description Function calls \code{\link[textmineR]{GetProbableTerms}} with some 
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
#' # make a dtm with unigrams and bigrams
#' data(nih_sample)
#' 
#' dtm <- CreateDtm(doc_vec = nih_sample$ABSTRACT_TEXT,
#'                  doc_names = nih_sample$APPLICATION_ID,
#'                  ngram_window = c(1,2))
#' 
#' # make a topic model
#' m <- FitLsaModel(dtm = dtm,
#'                  k = 10)
#'
#' assignments <- t(apply(m$theta, 1, function(x){
#'   x[ x < 0.05 ] <- 0
#'   x / sum(x)
#' }))
#' 
#' labels <- LabelTopics(assignments = assignments, dtm = dtm, M = 2)
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
  
  if(ncol(dtm_ngram) == 0){
    warning("dtm does not appear to contain ngrams. Using unigrams but ngrams will",
            " work much better.")
    dtm_ngram <- dtm
  }
  
  p_terms <- Matrix::colSums(dtm_ngram)
  p_terms <- p_terms / sum(p_terms)
  
  # apply the label algorithm over each topic
  result <- lapply(doc_list, function(x){
    l <- GetProbableTerms(docnames = x, dtm = dtm_ngram, p_terms = p_terms)
    names(l)[ order(l, decreasing=T) ][ 1:M ]
  })
  
  # format into a matrix for output
  result <- do.call(rbind, result)
  
  colnames(result) <- paste("label_", 1:ncol(result), sep="")
  
  result
}




#' Get Top Terms for each topic from a topic model
#'   
#' @description Takes topics by terms matrix and returns top M terms for each topic
#' @param phi A matrix whose rows index topics and columns index words
#' @param M An integer for the number of terms to return
#' @return Returns a \code{data.frame} whose columns correspond to a topic and
#' whose m-th row correspond to the m-th top term from the input \code{phi}.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm)
#' data(nih_sample_topic_model) 
#' 
#' top_terms <- GetTopTerms(phi = nih_sample_topic_model$phi, M = 5)
#' 
#' str(top_terms)

GetTopTerms <- function(phi, M){
  
  result <- apply(phi, 1, function(x){
    names(x)[ order(x, decreasing=TRUE) ][ 1:M ]
  })
  
  return(result)
}
