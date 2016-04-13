#' Represent a document clustering as a topic model
#' @description Represents a document clustering as a topic model of two matrices.
#' phi: P(term | cluster) theta: P(cluster | document)
#' @param dtm A document term matrix of class \code{dgCMatrix} or whose class 
#' inherits from the \code{Matrix} packge. Columns must index terms, rows must 
#' index documents.
#' @param clustering A vector of length \code{nrow(dtm)} whose entries form a
#' partitional clustering of the documents.
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}.
#' @return Returns a list with two elements, phi and theta. 'phi' is a matrix 
#' whose j-th row represents P(terms | cluster_j). 'theta' is a matrix whose
#' j-th row represents P(clusters | document_j). Each row of theta should only
#' have one non-zero element.
#' @examples
#' \dontrun{
#' # Load pre-formatted data for use
#' data(nih_sample_dtm)
#' data(nih_sample) 
#' 
#' result <- Cluster2TopicModel(dtm = nih_sample_dtm, 
#'                              clustering = nih_sample$IC_NAME)
#' }

#' @export 
Cluster2TopicModel <- function(dtm, clustering, ...){
  
  # Check inputs
  # TO DO
  
  # Set up some objects 
  iterator <- data.frame(id = rownames(dtm), 
                         clust = clustering, 
                         stringsAsFactors = FALSE)
  
  iterator <- by(iterator, INDICES = iterator$clust, function(x){
    docs <- x$id
    clust <- x$clust[ 1 ]
    list(docs = docs, clust = clust)
  })
  
  cnames <- unique(clustering)
  
  # Get theta
  theta <- TmParallelApply(X = iterator, FUN = function(x){
    m <- matrix(0, nrow = length(x$docs), ncol = length(cnames))
    rownames(m) <- x$docs
    colnames(m) <- cnames
    
    m[ , as.character(x$clust) ] <- 1
    
    m
  }, ...)
  
  theta <- do.call(rbind, theta)
  
  # if cluster names appear to be numeric, append "c" to them
  if(sum(grepl("[^0-9]", colnames(theta))) == 0 ){ 
    colnames(theta) <- paste("c", colnames(theta), sep = "_")
  }
  
  # sort rows of theta to line up with the dtm
  theta <- theta[ rownames(dtm) , ]
  
  # Get phi
  
  phi <- TmParallelApply(X = iterator, FUN = function(x){
    sub <- dtm[ x$docs , ]
    
    if( ! is.null(dim(sub))){
      Matrix::colSums(dtm[ x$docs , ]) / sum(Matrix::colSums(dtm[ x$docs , ]))
    }else{
      sub / sum(sub)
    }
    
  }, ...)
  
  phi <- do.call(rbind, phi)
  
  rownames(phi) <- colnames(theta)
  colnames(phi) <- colnames(dtm)
  
  return(list(theta = theta, phi = phi))
}