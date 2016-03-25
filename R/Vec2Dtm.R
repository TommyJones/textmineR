#' @title Convert a character vector to a document term matrix of class Matrix.
#' @description This is the main document term matrix creating function for \code{textmineR}.
#' In most cases, all you need to do is import documents as a character vector in R and then run this function
#' to get a document term matrix that is compatible with the rest of \code{textmineR}'s functionality.
#' 
#' @param vec A character vector of documents. Punctuation etc. is allowed. names(vec) should be names of your documents.
#' @param min.n.gram The minimum size of n for creating n-grams. (Defaults to 1)
#' @param max.n.gram The maximum size of n for creating n-grams. (Defaults to 1. Numbers greater than 3 are discouraged due to risk of overfitting.)
#' @param remove.stopwords Do you want to remove standard stopwords from your documents? (Defaults to TRUE)
#' @param custom.stopwords If not NULL (the default) a character vector of additional stopwords to remove from your corpus. Note: it is generally faster to
#' remove additional stopwords by subsetting the columns of a DTM post-hoc. This is to be used when you want these words removed before creatign the DTM.
#' @param lower Do you want all words coerced to lower case? (Defaults to TRUE)
#' @param remove.punctuation Do you want to convert all punctuation to spaces? For example, "big-lipped fish" goes to "big lipped fish" (Defaults to TRUE)
#' @param remove.numbers Do you want to convert all numbers to spaces? For example, "3rd grade teachers" goes to " rd grade teachers" (Defaults to TRUE)
#' @param stem.document Do you want to stem the words in your document? (Defaults to FALSE)
#' @param ... Additional arguments to be passed to \code{textmineR::TmParallelApply}
#' @note
#' This function relies heavily on the \code{tm} and \code{RWeka} packages. N-grams are derived using the \code{RWeka} package. 
#' There is a confilct between \code{RWeka} and \code{parallel} (used for multithread processing on unix-like systems).
#' Consequently, using n-grams (n > 1) causes construction of the DTM to be considerably slower on unix systems. 
#' Speed is unaffected on Windows machines because parallelization is not supported for DTM construction.
#'
#' @export
#' @examples
#' # Load some data and format it into a character vector
#' library(tm)
#' data(acq)
#' 
#' documents <- sapply(acq, function(x) x$content)
#' 
#' \dontrun{
#' # Make a DTM
#' dtm <- Vec2Dtm(vec = documents, min.n.gram = 1, max.n.gram = 2)
#' 
#' dim(dtm)
#' 
#' head(colnames(dtm))
#' 
#' head(rownames(dtm))
#' }
#' 


Vec2Dtm <- function(vec, docnames = names(vec), min.n.gram=1, max.n.gram=1, 
                    remove.stopwords=TRUE, custom.stopwords=NULL, lower=TRUE, 
                    remove.punctuation=TRUE, remove.numbers=TRUE, stem.document=FALSE,
                    ...){
  
  ### Convert arguments to conform to textmineR v2.0 naming scheme -------------
  doc_vec <- vec
  min_ngram <- min.n.gram
  max_ngram <- max.n.gram
  remove_stopwords <- remove.stopwords
  custom_stopwords <- custom.stopwords
  remove_punctuation <- remove.punctuation
  remove_numbers <- remove.numbers
  stem_documents <- stem.document

  ### Pre-process the documents ------------------------------------------------
  if(is.null(docnames)){
    warning("No document names detected. Assigning 1:length(doc_vec) as names.")
    docnames <- 1:length(doc_vec)
  }
  
  if (remove_stopwords) {
    stopwords <- unique(c(tm::stopwords("english"), tm::stopwords("SMART")))
  }
  else {
    stopwords <- c()
  }
  
  if (!is.null(custom_stopwords)){
    stopwords <- c(stopwords, custom.stopwords)
  } 
  
  if (lower) {
    doc_vec <- tolower(doc_vec)
    stopwords <- tolower(stopwords)
  }
  
  if (remove_punctuation) {
    doc_vec <- stringr::str_replace_all(doc_vec, "[^a-zA-Z0-9]", " ")
    stopwords <- stringr::str_replace_all(stopwords, "[^a-zA-Z0-9]", " ")
    stopwords <- unique(unlist(stringr::str_split(string = stopwords, 
                                                  pattern = "\\s+")))
  }
  
  if (remove_numbers) {
    doc_vec <- stringr::str_replace_all(doc_vec, "[0-9]", " ")
  }
  
  doc_vec <- stringr::str_replace_all(doc_vec, "\\s+", " ")
  
  ### Create iterators, vocabulary, other objects for dtm construction ---------
  
  # tokenize & construct vocabulary
  tokens <- text2vec::word_tokenizer(string = doc_vec)
  
  if(remove_stopwords | ! is.null(custom_stopwords)){
    tokens <- textmineR::TmParallelApply(X = tokens, FUN = function(x){
      setdiff(x, stopwords)
    }, export = "stopwords", ...)
  }
  
  
  if(stem_documents){
    tokens <- textmineR::TmParallelApply(X = tokens, FUN = function(x){
      SnowballC::wordStem(x, "porter")
    }, ...)
  }
  
  it <- text2vec::itoken(tokens)
  
  vocabulary <- text2vec::create_vocabulary(itoken_src = it, 
                                            ngram = c(ngram_min = as.integer(min_ngram),
                                                      ngram_max = as.integer(max_ngram)))
  
  
  vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
  
  ### Get the dtm, make sure it has correct dimnames, and return ---------------
  it <- text2vec::itoken(tokens)
  
  corpus <- text2vec::create_corpus(it, vectorizer)
  
  dtm <- text2vec::get_dtm(corpus, type = "dgCMatrix")
  
  rownames(dtm) <- docnames
  
  return(dtm)
}
