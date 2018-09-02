#' @title Function for reading text files into R
#'
#' @description This function reads in all files in a directory ending in .txt into R.
#' The result is a character vector where each entry is a .txt file.
#' The names of the resulting vector are derived from the names of the files.
#' @param directory A path to directory containing the files you want to read into R.
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}. See note, below.
#' @return 
#' Returns a character vector where each entry corresponds to a document.
#' @note
#' This function performs parallel computation by default. The default 
#' behavior is to use all available cores according to \code{\link[parallel]{detectCores}}.
#' However, this can be modified by passing the \code{cpus} argument when calling
#' this function.
#' @export
#' @examples
#' \dontrun{
#' my_text_vector <- Files2Vec("/path/to/my/data/")
#' }


Files2Vec <- function(directory, ...){
  .Deprecated(msg = "Files2Vec is slated for deletion in textmineR v3.0. Please plan accordingly.
              Please submit questions or requests to 	https://github.com/TommyJones/textmineR/issues")
  
    if( ! grepl("/$", directory) ) directory <- paste(directory, "/", sep="")
    
	file.list <- grep("\\.txt", dir(directory), value=T)
	
  vec <- TmParallelApply(X = file.list, 
                         FUN = function(x){
                           result <- scan(paste(directory, x, sep=""), what="character", sep="\n")
                           result <- paste(result, collapse="\n")
                           result <- gsub("\\s", " ", result)
                           return(result)
                         }, export = c("directory"), ...)
  
	vec <- unlist(vec)
	
	names(vec) <- gsub("[^a-zA-Z0-9]", "_", file.list)
	
	return(vec)
}
