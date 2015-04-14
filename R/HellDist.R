#' Hellinger Distance
#' @description Calculates the Hellinger distances between two discrete probability distributions p and q
#' @param p A numeric vector representing a probability distribution
#' @param q A numeric vector of length length(p) representing a probability distribuiton
#' @keywords distance functions
#' @export
#' @examples
#' x <- rnorm(n = 100, mean = 1, sd = 1)
#' y <- x^2
#' HellDist(p = x, q = y)r

HellDist = function(p,q){
     
  # don't divide by zero, we'll all die
  p[ p <= 0 ] <- 10^-4
  q[ q <= 0 ] <- 10^-4
    
  #make unit length
  p = p/sum(p)
  q = q/sum(q)
  
  # set up for vectorization
  m <- sqrt(p) - sqrt(q)
  
  m <- m * m
  
  result <- 1/sqrt(2) * sqrt(sum(m))
  
  return(result)
}

