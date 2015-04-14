  #' Jensen-Shannon Divergence
  #' @description This function calculates the Jensen Shannon Divergence for two probability vectors, p and q.
  #' @param p A numeric vector representing a probability distribution
  #' @param q A numeric vector of length length(p) representing a probability distribuiton
  #' @keywords distance functions
  #' @export
  #' @examples
  #' x <- rnorm(n = 100, mean = 1, sd = 1)
  #' y <- x^2
  #' JSD(p = x, q = y)


JSD<-function(p,q){    
  # don't divide by zero, we'll all die
  p[ p <= 0 ] <- 10^-4
  q[ q <= 0 ] <- 10^-4
  
  p <- p / sum(p)
  q <- q / sum(q)
  
  m=(p + q) / 2
    
  jsd <- (0.5 * sum(log(p / m) * p)) + (0.5 * sum(log(q / m) * q))
  
  return(jsd)
}
