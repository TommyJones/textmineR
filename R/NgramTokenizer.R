# Get n-grams when creating a document term matrix

NgramTokenizer <- function(min, max) {
  require(RWeka)  
  
  result <- function(x) {NGramTokenizer(x, Weka_control(min = min, max = max))}
  
  return(result)
}
