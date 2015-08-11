#' Hellinger Distance
#' @description Calculates the Hellinger distances or the rows or columns of a 
#' numeric matrix or for two numeric vectors.
#' @param x A numeric matrix or numeric vector 
#' @param y A numeric vector. \code{y} must be specified if \code{x} is a numeric vector.
#' @param by.rows Logical. If \code{x} is a matrix, should distances be calculated by rows?
#' @return If \code{x} is a matrix, this returns an square and symmetric matrix. 
#' The i,j entries correspond to the Hellinger Distance between the rows of \code{x} 
#' (or the columns of \code{x} if \code{by.rows = FALSE}). If \code{x} and \code{y}
#' are vectors, this returns a numeric scalar whose value is the Hellinger Distance
#' between \code{x} and \code{y}.
#' @keywords distance functions
#' @export
#' @examples
#' x <- rchisq(n = 100, df = 8)
#' y <- x^2
#' HellDist(x = x, y = y)
#' 
#' mymat <- rbind(x, y)
#' HellDist(x = mymat)

HellDist <- function(x, y=NULL, by.rows=TRUE){
     
  #############################################################################
  # case 1: x is not specified correctly
  #############################################################################
  
  if( ! is.numeric(x) | ! (is.matrix(x) | is.vector(x)) ){
    stop("x must be a numeric matrix or numeric vector. ")
  }
  
  #############################################################################
  # case 2: x is a numeric matrix
  #############################################################################
  if( is.matrix(x) & is.numeric(x) ){
    if( ! is.null(y) ){ # if you specified y, it's ignored and we warn
      warning("x is a numeric matrix, y is ignored")
    }
    
    if(! by.rows){
      x <- t(x)
    }
    
    result <- textmineR::HellingerMat(A = x) # this function only calculates the upper triangle
    
    result <- result + t(result)
    
    colnames(result) <- rownames(x)
    rownames(result) <- rownames(x)
  }
  
  #############################################################################
  # case 3: x is a numeric vector
  #############################################################################
  
  if( is.vector(x) & is.numeric(x) ){
    if(! (is.vector(y) & is.numeric(y)) ){ # if y isn't the right class
      stop("if x is a numeric vector, y must be a numeric vector of length x")
    }
    if(length(x) != length(y)){
      stop("x and y must be of the same length")
    }
    
    result <- textmineR::Hellinger_cpp(p = x, q = y)
  }
  
  result
}

