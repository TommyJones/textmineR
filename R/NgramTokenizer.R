# Get n-grams when creating a document term matrix

NgramTokenizer <- function(min, max) {
  
  result <- function(x){
    RWeka::NGramTokenizer(x, RWeka::Weka_control(min = min, max = max))
  }
  
  return(result)
}
