#' @title Get n-grams when creating a document term matrix
#' @description Function creates a function to create ngrams when converting a Corpus object to a DocumentTermMatrix object
#' @details For bigrams min=max=2. For bigrams and trigrams min=2, max=3
#' @param min The minimum size of n for your n-grams
#' @param max The maximum size of n for your n-grams
#' @export
#' @examples 
#' ngrams <- NgramTokenizer(1, 3) # for unigrams, bigrams, and trigrams
#' myDTM <- DocumentTermMatrix(myCorp, control = list(tokenize = ngrams))


NgramTokenizer <- function(min, max) {
  require(RWeka)  
  
  result <- function(x) {NGramTokenizer(x, Weka_control(min = min, max = max))}
  
  return(result)
}
