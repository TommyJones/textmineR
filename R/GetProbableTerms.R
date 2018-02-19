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
