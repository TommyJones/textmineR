#' Calculate phi.prime = P(topic|word)
#' 
#' @description This function takes a phi matrix (P(word|topic)) and a theta matrix (P(topic|document)) and 
#' returns phi.prime = P(topic|word). Phi.prime can be used for classifying new documents and for topic labels.
#' 
#' @param phi = the phi matrix whose rows index topics and columns index words. The i, j entries are P(word_i | topic_j)
#' @param theta = the theta matrix whose rows index documents and columns index topics. The i, j entries are P(topic_i | document_j)
#' 
#' @export
#' @examples
#' lda$phi.prime <- GetPhiPrime(phi=lda$phi, theta=lda$theta)
#' 


GetPhiPrime <- function(phi, theta){

    # set up constants
    D <- nrow(theta)
    K <- ncol(theta)
    V <- ncol(phi)
    
    # probability of each document (assumed to be equiprobable)
    p.d <- rep(1/nrow(theta), nrow(theta))
    
    # get the probability of each topic
    p.t <- p.d %*% theta
    
    # get the probability of each word from the model    
    p.w <- p.t %*% phi
    

    
    # get our result
    phi.prime <- matrix(0, ncol=ncol(p.t), nrow=ncol(p.t))
    diag(phi.prime) <- p.t
    
    phi.prime <- phi.prime %*% phi
    
    phi.prime <- t(apply(phi.prime, 1, function(x) x / p.w))
    
    rownames(phi.prime) <- rownames(phi)
    colnames(phi.prime) <- colnames(phi)
    
    return(phi.prime)
}
