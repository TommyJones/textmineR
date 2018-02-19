#' Probabilistic coherence of topics
#' @description Calculates the probabilistic coherence of a topic or topics. 
#' This approximates semantic coherence or human understandability of a topic.
#' @param phi A numeric matrix or a numeric vector. The vector, or rows of the 
#' matrix represent the numeric relationship between topic(s) and terms. For
#' example, this relationship may be p(word|topic) or p(topic|word).
#' @param dtm A document term matrix  or co-occurrence matrix of class 
#' \code{matrix} or whose class inherits from the \code{Matrix} package. Columns
#' must index terms.
#' @param M An integer for the number of words to be used in the calculation. 
#' Defaults to 5
#' @return Returns an object of class \code{numeric} corresponding to the 
#' probabilistic coherence of the input topic(s).
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_topic_model)
#' data(nih_sample_dtm) 
#' 
#' CalcProbCoherence(phi = nih_sample_topic_model$phi, dtm = nih_sample_dtm, M = 5)
#' @export 
CalcProbCoherence<- function(phi, dtm, M = 5){
  
  # phi is a numeric matrix or numeric vector?
  if( ! is.numeric(phi) ){
    stop("phi must be a numeric matrix whose rows index topics and columns\n",
         " index terms or phi must be a numeric vector whose entries index terms.")
  }
  # is dtm a matrix we can work with?
  if( ! is.matrix(dtm) & 
      ! class(dtm) %in% c("dgCMatrix", "dgTMatrix", "dgeMatrix", "dgRMatrix") ){
    stop("dtm must be a matrix. This can be a standard R dense matrix or a\n",
         " matrix of class dgCMatrix, dgTMatrix, dgRMatrix, or dgeMatrix")
  }
  
  # is M numeric? If it is not an integer, give a warning.
  if( ! is.numeric(M) | M < 1){
    stop("M must be an integer in 1:ncol(phi) or 1:length(phi)")
  }
  
  if(length(M) != 1){
    warning("M is a vector when scalar is expected. Taking only the first value")
    M <- M[ 1 ]
  }
  
  if(floor(M) != M){
    warning("M is expected to be an integer. floor(M) is being used.")
    M <- floor(M)
  }
  
  # dtm has colnames?
  if( is.null(colnames(dtm))){
    stop("dtm must have colnames")
  }
  
  # Names of phi in colnames(dtm)
  if( ! is.matrix(phi) ){
    if(sum(names(phi)[ 1:M ] %in% colnames(dtm)) != length(1:M)){
      stop("names(phi)[ 1:M ] are not in colnames(dtm)")
    }
  }else if(sum(colnames(phi)[ 1:M ] %in% colnames(dtm)) != length(1:M)){
    stop("colnames(phi)[ 1:M ] are not in colnames(dtm)")
  }

  # Declare a function to get probabilistic coherence on one topic
  pcoh <- function(topic, dtm, M){
    terms <- names(topic)[order(topic, decreasing = TRUE)][1:M]
    dtm.t <- dtm[, terms]
    dtm.t[dtm.t > 0] <- 1
    count.mat <- Matrix::t(dtm.t) %*% dtm.t
    num.docs <- nrow(dtm)
    p.mat <- count.mat/num.docs
    # result <- sapply(1:(ncol(count.mat) - 1), function(x) {
    #   mean(p.mat[x, (x + 1):ncol(p.mat)]/p.mat[x, x] - Matrix::diag(p.mat)[(x + 
    #                                                                           1):ncol(p.mat)], na.rm = TRUE)
    # })
    # mean(result, na.rm = TRUE)
    result <- sapply(1:(ncol(count.mat) - 1), function(x) {
      p.mat[x, (x + 1):ncol(p.mat)]/p.mat[x, x] - 
        Matrix::diag(p.mat)[(x + 1):ncol(p.mat)]
    })
    mean(unlist(result), na.rm = TRUE) 
  }
  
  # if phi is a single topic vector get that one coherence
  if( ! is.matrix(phi) ){
    return(pcoh(topic = phi, dtm = dtm, M = M))
  }
  
  # Otherwise, do it for all the topics
  apply(phi, 1, function(x){
    pcoh(topic = x, dtm = dtm, M = M)
  })
}


