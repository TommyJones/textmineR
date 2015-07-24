#' @title Subset a document term matrix
#' @description This function is designed to work with an in-development shiny app. It's overkill to use on its own but you can if you want to...
#' @param dtm A document term matrix of class Matrix
#' @param keep.terms A character vector of colnames(dtm) indicating which terms you want to keep.
#' @export
#' 




SubsetDtm <- function(dtm, keep.terms){
	dtm <- dtm[ , keep.terms ]
	return(dtm)
}
