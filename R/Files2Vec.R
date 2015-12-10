#' @title Function for reading text files into R
#'
#' @description This function reads in all files in a directory ending in .txt into R.
#' The result is a character vector where each entry is a .txt file.
#' The names of the resulting vector are derived from the names of the files.
#'
#' @param directory A path to directory containing the files you want to read into R.
#' @return 
#' Returns a character vector where each entry corresponds to a document.
#' @export
#' @examples
#' \dontrun{
#' my_text_vector <- Files2Vec("/path/to/my/data/")
#' }


Files2Vec <- function(directory){
    
    if( ! grepl("/$", directory) ) directory <- paste(directory, "/", sep="")
    
	file.list <- grep("\\.txt", dir(directory), value=T)
	
  vec <- TmParallelApply(X = file.list, 
                         FUN = function(x){
                           result <- scan(paste(directory, x, sep=""), what="character", sep="\n")
                           result <- paste(result, collapse="\n")
                           result <- gsub("\\s", " ", result)
                           return(result)
                         }, export = c("directory"))
  
	vec <- unlist(vec)
	
	names(vec) <- gsub("[^a-zA-Z0-9]", "_", file.list)
	
	return(vec)
}
