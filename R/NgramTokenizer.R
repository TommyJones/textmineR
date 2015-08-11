# Get n-grams when creating a document term matrix
# This function is undocumented/internal to the package and not designed to be
# called by users.
NgramTokenizer <- function(min, max) {
  
  result <- function(x){
    RWeka::NGramTokenizer(x, RWeka::Weka_control(min = min, max = max))
  }
  
  return(result)
}
