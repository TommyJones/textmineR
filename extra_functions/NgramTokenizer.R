#' @title Get n-grams when creating a document term matrix
#' @description This function is a helper function for \code{textmineR}. It is
#' not designed for users. It aids in creating n-grams in DTM construction
#' @param min Smallest n-gram desired
#' @param max Largest n-gram desired
#' @return Returns a function
#' @export
#' @examples
#' \dontrun{
#' my_tokenizer <- NgramTokenizer(min=1, max=3)
#' 
#' dtm <- tm::DocumentTermMatrix(corp, control=list(tokenize=my_tokenizer))
#' 
#' dtm <- MakeSparseDTM(dtm)
#' }
# called by users.
NgramTokenizer <- function(min, max) {
  
  result <- function(x){
    RWeka::NGramTokenizer(x, RWeka::Weka_control(min = min, max = max))
  }
  
  return(result)
}
