#' @name CalcLikelihood
#' @title Calculate the log likelihood of a document term matrix given a topic model
#' @param dtm The document term matrix of class \code{dgCMatrix}.
#' @param phi The phi matrix whose rows index topics and columns index words. 
#' The i, j entries are P(word_i | topic_j)  
#' @param theta The theta matrix whose rows index documents and columns index topics. 
#' The i, j entries are P(topic_i | document_j)
#' @param ... Other arguments to pass to \code{\link[textmineR]{TmParallelApply}}. See note, below. 
#' @description
#'     This function takes a DTM, phi matrix (P(word|topic)), and a theta matrix 
#'     (P(topic|document)) and returns a single value for the likelihood of the 
#'     data given the model.     
#' @return
#' Returns an object of class \code{numeric} corresponding to the log likelihood.
#' @note
#' This function performs parallel computation if \code{dtm} has more than 3,000
#' rows. The default is to use all available cores according to \code{\link[parallel]{detectCores}}.
#' However, this can be modified by passing the \code{cpus} argument when calling
#' this function.
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm) 
#' data(nih_sample_topic_model)
#' 
#' # Get the likelihood of the data given the fitted model parameters
#' ll <- CalcLikelihood(dtm = nih_sample_dtm, 
#'                      phi = nih_sample_topic_model$phi, 
#'                      theta = nih_sample_topic_model$theta)
#' 
#' ll
#' @export
CalcLikelihood <- function(dtm, phi, theta, ...){
  
  # check that inputs have necessary formats
  if(nrow(dtm) != nrow(theta) ){ 
    # number of documents matches?
    stop("nrow(dtm) must match nrow(theta).")
  }
  
  if(nrow(phi) != ncol(theta)){ 
    # number of topics matches?
    stop("nrow(phi) must match ncol(theta)")
  }
  
  if(ncol(dtm) != ncol(phi)){ 
    # vocabulary size matches?
    stop("ncol(dtm) must match ncol(phi)")
  }
  
  if(is.null(rownames(dtm)) | is.null(rownames(theta))){ 
    # doc names exist?
    warning("missing rownames from one or both of dtm and theta. Row index used instead.")
    rownames(dtm) <- 1:nrow(dtm)
    rownames(theta) <- 1:nrow(theta)
  }
  
  if(is.null(colnames(dtm)) | is.null(colnames(phi))){ 
    # term names exist?
    warning("missing colnames from one or both of dtm and phi. Column index used instead")
    colnames(dtm) <- 1:ncol(dtm)
    colnames(phi) <- 1:ncol(phi)
  }
  
  if(is.null(rownames(phi)) | is.null(colnames(theta))){ 
    # topic names exist?
    warning("missing colnames from theta or rownames from phi. Row/column indeces used instead.")
    colnames(theta) <- 1:ncol(theta)
    rownames(phi) <- 1:nrow(phi)
  }
  
  if(sum(intersect(colnames(dtm), colnames(phi)) %in% union(colnames(dtm), colnames(phi))) != ncol(dtm)){
    # all terms in dtm present in phi?
    stop("vocabulary does not match between dtm and phi. Check colnames of both matrices.")
  }
  
  if(sum(intersect(rownames(dtm), rownames(theta)) %in% union(rownames(dtm), rownames(theta))) != nrow(dtm)){
    # all documents in dtm present in theta?
    stop("document names do not match between dtm and theta. Check rownames of both matrices.")
  }
  
  if(sum(intersect(colnames(theta), rownames(phi)) %in% union(colnames(theta), rownames(phi))) != ncol(theta)){
    # all topics in theta present in phi?
    stop("topic names do not match. Check rownames(phi) and colnames(theta)")
  }
  
  # ensure that all inputs are sorted correctly
  phi <- phi[ colnames(theta) , colnames(dtm) ]
  
  theta <- theta[ rownames(dtm) , ]
  
  # do in parallel in batches of about 3000 if we have more than 3000 docs
  if(nrow(dtm) > 3000){
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    data_divided <- lapply(batches, function(j){
      
      dtm_divided <- dtm[ j:min(j + 2999, nrow(dtm)) , ]
      
      theta_divided <- theta[ j:min(j + 2999, nrow(dtm)) , ]
      
      list(dtm_divided=dtm_divided, theta_divided=theta_divided)
    })
    
    result <-TmParallelApply(X = data_divided, FUN = function(x){
      CalcLikelihoodC(dtm=x$dtm_divided, 
                      phi=phi, 
                      theta=x$theta_divided)
    }, export=c("phi"), ...)
    
    result <- sum(unlist(result))
    
    # do sequentially otherwise
  }else{
    result <- CalcLikelihoodC(dtm=dtm, phi=phi, theta=theta)
  }
  
  result  
}


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


#' Calculate the R-squared of a topic model.
#' @description Function to calculate R-squared for a topic model. 
#' This uses a geometric interpretation of R-squared as the proportion of total distance 
#' each document is from the center of all the documents that is explained by the model. 
#' @param dtm A documents by terms dimensional document term matrix of class
#' \code{dgCMatrix} or of class \code{matrix}. 
#' @param phi A topics by terms dimensional matrix where each entry is p(term_i |topic_j)
#' @param theta A documents by topics dimensional matrix where each entry is p(topic_j|document_d)
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}. See note, below.
#' @return
#' Returns an object of class \code{numeric} representing the proportion of variability
#' in the data that is explained by the topic model.
#' @note
#' This function performs parallel computation if \code{dtm} has more than 3,000
#' rows. The default is to use all available cores according to \code{\link[parallel]{detectCores}}.
#' However, this can be modified by passing the \code{cpus} argument when calling
#' this function.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm) 
#' data(nih_sample_topic_model)
#' 
#' # Get the R-squared of the model
#' r2 <- CalcTopicModelR2(dtm = nih_sample_dtm, 
#'                      phi = nih_sample_topic_model$phi, 
#'                      theta = nih_sample_topic_model$theta)
#' 
#' 
#' r2
CalcTopicModelR2 <- function(dtm, phi, theta, ...){
  
  # check that inputs have necessary formats
  if(nrow(dtm) != nrow(theta) ){ 
    # number of documents matches?
    stop("nrow(dtm) must match nrow(theta).")
  }
  
  if(nrow(phi) != ncol(theta)){ 
    # number of topics matches?
    stop("nrow(phi) must match ncol(theta)")
  }
  
  if(ncol(dtm) != ncol(phi)){ 
    # vocabulary size matches?
    stop("ncol(dtm) must match ncol(phi)")
  }
  
  if(is.null(rownames(dtm)) | is.null(rownames(theta))){ 
    # doc names exist?
    warning("missing rownames from one or both of dtm and theta. Row index used instead.")
    rownames(dtm) <- 1:nrow(dtm)
    rownames(theta) <- 1:nrow(theta)
  }
  
  if(is.null(colnames(dtm)) | is.null(colnames(phi))){ 
    # term names exist?
    warning("missing colnames from one or both of dtm and phi. Column index used instead")
    colnames(dtm) <- 1:ncol(dtm)
    colnames(phi) <- 1:ncol(phi)
  }
  
  if(is.null(rownames(phi)) | is.null(colnames(theta))){ 
    # topic names exist?
    warning("missing colnames from theta or rownames from phi. Row/column indeces used instead.")
    colnames(theta) <- 1:ncol(theta)
    rownames(phi) <- 1:nrow(phi)
  }
  
  if(sum(intersect(colnames(dtm), colnames(phi)) %in% union(colnames(dtm), colnames(phi))) != ncol(dtm)){
    # all terms in dtm present in phi?
    stop("vocabulary does not match between dtm and phi. Check colnames of both matrices.")
  }
  
  if(sum(intersect(rownames(dtm), rownames(theta)) %in% union(rownames(dtm), rownames(theta))) != nrow(dtm)){
    # all documents in dtm present in theta?
    stop("document names do not match between dtm and theta. Check rownames of both matrices.")
  }
  
  if(sum(intersect(colnames(theta), rownames(phi)) %in% union(colnames(theta), rownames(phi))) != ncol(theta)){
    # all topics in theta present in phi?
    stop("topic names do not match. Check rownames(phi) and colnames(theta)")
  }
  
  # ensure that all inputs are sorted correctly
  phi <- phi[ colnames(theta) , colnames(dtm) ]
  
  theta <- theta[ rownames(dtm) , ]
  
  
  # get ybar, the "center" of the documents
  ybar <- Matrix::colMeans(dtm)
  
  # do in parallel in batches of about 3000 if we have more than 3000 docs
  if(nrow(dtm) > 3000){
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    data_divided <- lapply(batches, function(j){
      
      dtm_divided <- dtm[ j:min(j + 2999, nrow(dtm)) , ]
      
      theta_divided <- theta[ j:min(j + 2999, nrow(dtm)) , ]
      
      list(dtm_divided=dtm_divided, theta_divided=theta_divided)
    })
    
    result <-TmParallelApply(X = data_divided, FUN = function(x){
      CalcSumSquares(dtm = x$dtm_divided, 
                     phi = phi, 
                     theta = x$theta_divided, 
                     ybar=ybar)
    }, export=c("phi", "ybar"), ...)
    
    result <- do.call(rbind, result)
    
    result <- 1 - sum(result[ , 1 ]) / sum(result[ , 2 ])
    
    # do sequentially otherwise
  }else{
    sum_squares <- CalcSumSquares(dtm = dtm,  phi = phi, theta = theta, ybar=ybar)
    
    result <- 1 - sum_squares[ 1 ] / sum_squares[ 2 ]
    
  }
  
  result
}