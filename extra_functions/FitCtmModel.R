

FitCtmModel <- function(dtm, k, return_all = TRUE, ...){
  
  model <- topicmodels::CTM(x = dtm, k = k, ...)
  
  theta <- model@gamma
  
  rownames(theta) <- model@documents
  colnames(theta) <- paste("t", 1:ncol(theta), sep = "_")
  
  phi <- exp(model@beta)
  colnames(phi) <- model@terms
  rownames(phi) <- colnames(theta)
  
  result <- list(theta = theta, phi = phi)
  
  if(return_all){
    result <- c(result, etc = model)
  }
  
  return(result)
}