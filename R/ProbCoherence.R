#' Probailistic coherence of topics
#' @description Calculates the probabilistic coherence of a topic. This 
#' approximates semmantic coherence or human understandability of a topic.
#' @param topic A probability vector denoting a relationship between words 
#' and a topic. This may be p(word|topic) or p(topic|word)
#' @param dtm A document term matrix of class Matrix. Columns index words; 
#' rows index documents
#' @param M An integer for the number of words to be used in the calculation. 
#' Defaults to 5
#' @return Returns an object of class \code{numeric} corresponding to the 
#' probabilistic coherence of the input topic.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_topic_model) 
#' data(nih_sample_dtm)
#' 
#' # Coherence of topic 1
#' ProbCoherence(topic = nih_sample_topic_model$phi[ 1 , ], dtm = nih_sample_dtm, M = 5)
#' 

ProbCoherence <- function( topic, dtm, M=5){
  .Deprecated(new = "CalcProbCoherence", package = "textmineR",
              msg = "ProbCoherence is deprecated and will be removed in textmineR v2.0.0
              Use 'CalcProbCoherence' instead.",
              old = "ProbCoherence")

## TODO: consider changing probability calculations from document frequency to term frequency
      
  # ordered vector of most probable M terms given a topic
  terms <- names(topic)[ order(topic, decreasing=TRUE ) ][ 1:M ]
  
  # sparse subset of dtm for terms, columns ordered by decreasing probability
  dtm.t <- dtm[ , terms ]
  dtm.t[ dtm.t > 0 ] <- 1
  count.mat <- Matrix::t(dtm.t) %*% dtm.t
  
  num.docs <- nrow(dtm)
  
  p.mat <- count.mat / num.docs
  
  
  result <- sapply( 1:(ncol(count.mat) - 1), function(x){
    
    mean(p.mat[ x, (x + 1):ncol(p.mat) ]/p.mat[ x , x ] - Matrix::diag(p.mat)[ (x + 1):ncol(p.mat) ], na.rm=TRUE)
    
  })
  
  return( mean(result, na.rm=TRUE) )
}
