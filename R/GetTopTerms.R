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
