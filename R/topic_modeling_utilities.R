#' Summarize topics in a topic model
#' @description Create a data frame summarizing the contents of each topic in a model
#' @param model A list (or S3 object) with three named matrices: phi, theta, and gamma.
#'        These conform to outputs of many of \link[textmineR]{textmineR}'s native
#'        topic modeling functions such as \link[textmineR]{FitLdaModel}. 
#' @return An object of class \code{data.frame} with 6 columns: 'topic' is the 
#'         name of the topic, 'prevalence' is the rough prevalence of the topic 
#'         in all documents across the corpus, 'coherence' is the probabilistic
#'         coherence of the topic, 'top_terms_phi' are the top 5 terms for each
#'         topic according to P(word|topic), 'top_terms_gamma' are the top 5 terms
#'         for each topic according to P(topic|word).
#' @details 'prevalence' is normalized to sum to 100. If your 'theta' matrix has
#'          negative values (as may be the case with an LSA model), a constant is
#'          added so that the least prevalent topic has a prevalence of 0.
#'          
#'          'coherence' is calculated using \link[textmineR]{CalcProbCoherence}.
#'          
#'          'label' is assigned using the top label from \link[textmineR]{LabelTopics}.
#'          This requires an "assignment" matrix. This matrix is like a "theta" matrix
#'          except that it is binary. A topic is "in" a document or it is not.
#'          The assignment is made by comparing each value of theta to the minimum
#'          of the largest value for each row of theta (each document). This 
#'          ensures that each document has at least one topic assigned to it.
#' @examples
#' \dontrun{
#' SummarizeTopics(nih_sample_topic_model)
#' }
#' @export
SummarizeTopics <- function(model){
  
  # check inputs
  if (! "phi" %in% names(model) | ! "theta" %in% names(model)) {
    stop("model must contain a 'phi' matrix and a 'theta' matrix named as such.")
  }
  
  # get coherence
  if ("coherence" %in% names(model)) {
    coherence <- model$coherence
  } else {
    coherence <- CalcProbCoherence(model$phi, model$data)
  }
  
  # get prevalence - this gets fancy to account for negatives in LSA models
  p <- colSums(model$theta)
  
  if (sum(p < 0) > 0) { # if there are any negatives
    p <- p + min(p)
  }
  
  prevalence <- p / sum(p) * 100
  
  # get top terms from phi
  tt_phi <- GetTopTerms(phi = model$phi, M = 5)
  
  tt_phi <- apply(tt_phi, 2, function(x){
    paste(x, collapse = ", ")
  })
  
  # get top terms from gamma
  tt_gamma <- GetTopTerms(phi = model$gamma, M = 5)
  
  tt_gamma <- apply(tt_gamma, 2, function(x){
    paste(x, collapse = ", ")
  })
  
  # get labels
  m <- apply(model$theta, 1, max, na.rm = TRUE)
  
  m <- min(m, na.rm = TRUE)
  
  a <- model$theta >= m # (mean(model$theta) + 0 * sd(model$theta))
  
  labels <- LabelTopics(a, model$data, M = 1)
  
  # prepare output
  out <- data.frame(topic = rownames(model$phi),
                    label = labels,
                    prevalence = round(prevalence,2),
                    coherence = round(coherence,3),
                    top_terms_phi = tt_phi,
                    top_terms_gamma = tt_gamma,
                    stringsAsFactors = FALSE)
  
  out
}



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
#' data(nih_sample_topic_model)
#' 
#' m <- nih_sample_topic_model
#'
#' assignments <- t(apply(m$theta, 1, function(x){
#'   x[ x < 0.05 ] <- 0
#'   x / sum(x)
#' }))
#' 
#' assignments[is.na(assignments)] <- 0
#'
#' labels <- LabelTopics(assignments = assignments, dtm = m$data, M = 2)
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
    message("dtm does not appear to contain ngrams. Using unigrams but ngrams will",
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
