FormatRawStmOutput <- function(stm_result, docnames, vocab){
  theta <- stm_result$theta
  rownames(theta) <- docnames
  colnames(theta) <- paste("t", 1:ncol(theta), sep="_")
  
  phi <- exp(stm_result$beta$logbeta[[ 1 ]])
  rownames(phi) <- colnames(theta)
  colnames(phi) <- vocab
  
  etc <- stm_result[ setdiff(names(stm_result), c("theta", "beta")) ]
  
  return(list(theta = theta, phi = phi, etc = etc))
}