
#' Calculate Hellinger Distance
#' @description Calculates the Hellinger distances or the rows or columns of a 
#' numeric matrix or for two numeric vectors.
#' @param x A numeric matrix or numeric vector 
#' @param y A numeric vector. \code{y} must be specified if \code{x} is a numeric vector.
#' @param by_rows Logical. If \code{x} is a matrix, should distances be calculated by rows?
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}. See note, below
#' @return If \code{x} is a matrix, this returns an square and symmetric matrix. 
#' The i,j entries correspond to the Hellinger Distance between the rows of \code{x} 
#' (or the columns of \code{x} if \code{by_rows = FALSE}). If \code{x} and \code{y}
#' are vectors, this returns a numeric scalar whose value is the Hellinger Distance
#' between \code{x} and \code{y}.
#' @note
#' This function executes in parallel automatically for matrices. CPU usage should be no more than 2 cores. 
#' To execute sequentially (to save memory, for example), pass \code{cpus = 1} to the function call.
#' @export
#' @examples
#' x <- rchisq(n = 100, df = 8)
#' y <- x^2
#' CalcHellingerDist(x = x, y = y)
#' 
#' mymat <- rbind(x, y)
#' CalcHellingerDist(x = mymat)

CalcHellingerDist2 <- function(x, y=NULL, by_rows=TRUE, ...){
  
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
    
    if(! by_rows){
      x <- t(x)
    }
    
    # Get two batches of matrix rows
    batches <- seq(1, ceiling(nrow(x) / 2))
    
    batches <- list(x[ batches , ],
                    x[ -batches , ])
    
    # In parallel, calculate distance between rows by batch
    dist1 <- textmineR::TmParallelApply(X = batches,
                                        FUN = function(y) textmineR::HellingerMat(A = y),
                                        ...)
    
    # sequentially, calculate distance between the rows in batch 1 and 2
    
    dist3 <- textmineR::HellingerMat2Mat(A = batches[[ 1 ]],
                                         B = batches[[ 2 ]])
    
    dist3 <- do.call(rbind, dist3)
    
    # re-combine into a single upper-triangular matrix
    result <- rbind(cbind(dist1[[ 1 ]], dist3),
                    cbind(matrix(0, nrow = nrow(dist1[[ 1 ]]), ncol = ncol(dist1[[ 1 ]])),
                          dist1[[ 2 ]]))
    
    # add it to its transpose for the result
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