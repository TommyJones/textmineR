#' Calculate a matrix whose rows represent P(topic_i|tokens)
#' 
#' @description This function takes a phi matrix (P(token|topic)) and a theta 
#' matrix (P(topic|document)) and returns the phi prime matrix (P(topic|token)). 
#' Phi prime can be used for classifying new documents and for alternative
#' topic labels. This function is deprecated. Use \code{\link[textmineR]{CalcPhiPrime}} instead.
#' 
#' @param phi = The phi matrix whose rows index topics and columns index words. The i, j entries are P(word_i | topic_j)
#' @param theta = The theta matrix whose rows index documents and columns index topics. The i, j entries are P(topic_i | document_j)
#' @return
#' Returns a \code{matrix} whose rows correspond to topics and whose columns
#' correspond to tokens. The i,j entry corresponds to P(topic_i|token_j)
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_topic_model) 
#' 
#' # Make a phi_prime matrix, P(topic|words)
#' phi_prime <- GetPhiPrime(phi = nih_sample_topic_model$phi, 
#'                          theta = nih_sample_topic_model$theta)
#' 
GetPhiPrime <- function(phi, theta){
  .Deprecated(new = "CalcPhiPrime", package = "textmineR",
              msg = "GetPhiPrime is deprecated and will be removed in textmineR v3.0
              Use 'CalcPhiPrime' instead.",
              old = "GetPhiPrime")

  phi.prime <- CalcPhiPrime(phi = phi, theta = theta)
    
    return(phi.prime)
}
