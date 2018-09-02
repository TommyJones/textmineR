#' @title Function to remove some forms of pluralization. 
#' @description This function takes a character vector as input and removes some
#' forms of pluralization from the ends of the words. 
#' @param term_vec A character vector
#' 
#' @details The entries of the vector should be single words or short n-grams 
#' without punctuation as the function only looks at the ends of strings. In 
#' other words, if entries are a paragraph of text. Only the final words will 
#' get de-pluralized. (Even then, if the final character is a period, as would 
#' be the case with paragraphs, it's likely that nothing will be de-pluralized.)
#' @note WARNING: This does make mistakes for irregular words. You should check 
#' its results manually. It tends to fail spectacularly for words ending in "es".
#' @export
#' @return
#' Returns an object of class \code{data.frame} with three columns. The first
#' column is the argument \code{term_vec}. The second column is the depluralized
#' version of the words in \code{term_vec}. The third column is a logical, indicating
#' whether or not the word in \code{term_vec} was changed.
#' @examples
#' myvec <- c("banana", "bananas", "scientists", "large_armies")
#'
#' CorrectS(term_vec=myvec)
#'


CorrectS <- function(term_vec){
  .Deprecated(msg = "CorrectS is slated for deletion in textmineR v3.0. Please plan accordingly.
              Please submit questions or requests to 	https://github.com/TommyJones/textmineR/issues")
  
	# makes some adjustments to pluralization
	# WARNING: This does make mistakes for irregular words. You should check its results manually.
    s.adjust <- gsub("sses$", "ss", term_vec) 
    keep.list <- s.adjust[ grepl("sis$|ss$|us$|species$", s.adjust) | nchar(term_vec) <= 3 | grepl( "_[a-zA-Z][a-zA-Z][a-zA-Z]$", s.adjust) ]
    
    s.adjust2 <- gsub("ies$", "y", s.adjust)
    s.adjust2 <- gsub("s$", "", s.adjust2)
    
    out.list <- s.adjust2
    out.list[ s.adjust %in% keep.list ] <- s.adjust[ s.adjust %in% keep.list ]
    
    result <- data.frame(original=term_vec, adjusted=out.list, changed=term_vec!=out.list, stringsAsFactors=FALSE)
    return(result)
}
