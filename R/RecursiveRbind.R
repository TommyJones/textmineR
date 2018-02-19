#' @title Recursively call rBind from the Matrix package.
#' @description This is used for combining a list of sparse matrices into a large Matrix.
#' This is a worker function for \code{textmineR} and generally not made to be
#' used by users. However, the function is provided for custom functions. 
#' @param matrix_list A list, each element containing a matrix of class \code{dgCMatrix}
#' @return
#' Returns a matrix of class \code{dgCMatrix}. 
#' @examples
#' a_matrix <- Matrix::Matrix(0, nrow=10, ncol=10)
#' 
#' a_list <- list(a_matrix, a_matrix, a_matrix, a_matrix, a_matrix)
#' 
#' result <- RecursiveRbind(a_list)
#' 
#' @export 

RecursiveRbind <- function(matrix_list){
  
  if(length(matrix_list) <= 100){
    
    return(do.call(Matrix::rBind, matrix_list))
    
  }else{
    batches <- seq(1, length(matrix_list), by = 100)
    
    matrix_list <- lapply(batches, function(x){
      do.call(Matrix::rBind, matrix_list[ x:min(x + 99, length(matrix_list))])
    })
    
    RecursiveRbind(matrix_list=matrix_list)
  }
  
}


