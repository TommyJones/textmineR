#' Get Top Terms for each topic from a topic model
#'   
#' @description Takes topics X terms matrix and returns top M terms for each topic
#' @param phi A matrix whose rows index topics and columns index words
#' @param M An integer for the number of terms to return
#' @export
#' @examples
#' GetTopTerms(phi=my_phi, M=5)

GetTopTerms <- function(phi, M){
  
  result <- apply(phi, 1, function(x){
    names(x)[ order(x, decreasing=TRUE) ][ 1:M ]
  })
  
  return(result)
}
