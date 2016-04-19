#' @title Convert a character vector to a document term matrix of class Matrix.
#' @description This is the main document term matrix creating function for \code{textmineR}.
#' In most cases, all you need to do is import documents as a character vector in R and then 
#' run this function to get a document term matrix that is compatible with the 
#' rest of \code{textmineR}'s functionality and many other libraries. \code{vec2dtm}
#' is built on top of the excellent \code{link[text2vec]{text2vec}}library.
#' 
#' @param doc_vec A character vector of documents. 
#' @param docnames A vector of names for your documents. Defaults to 
#'        \code{names(doc_vec)}. If NULL, then docnames is set to be 
#'        \code{1:length(doc_vec)}.
#' @param min_ngram The minimum size of n for creating n-grams. Defaults to 1.
#' @param max_ngram The maximum size of n for creating n-grams. Defaults to 1. 
#'        Numbers greater than 3 are discouraged due to risk of overfitting.
#' @param remove_stopwords Do you want to remove standard stopwords from your documents? 
#'        Defaults to \code{TRUE}.
#' @param custom_stopwords If not \code{NULL} (the default) a character vector 
#'        of stopwords to remove from your corpus. 
#' @param lower Do you want all words coerced to lower case? Defaults to \code{TRUE}
#' @param remove_punctuation Do you want to convert all non-alpha numeric 
#'        characters to spaces? Defaults to \code{TRUE}
#' @param remove_numbers Do you want to convert all numbers to spaces? Defaults 
#'        to \code{TRUE}
#' @param stem_documents Do you want to stem the words in your document using 
#'        Porter's word stemmer? Defaults to \code{FALSE}
#' @param ... Other arguments to be passed to \code{textmineR::TmParallelApply}.
#' @return A document term matrix of class \code{dgCMatrix}. The rows index 
#' documents. The columns index terms. The i, j entries represent the count of 
#' term j appearing in document i.
#' @examples
#' \dontrun{
#' data(nih_sample)
#' 
#' 
#' dtm <- Vec2Dtm(doc_vec = nih_sample$ABSTRACT_TEXT,
#'                docnames = nih_sample$APPLICATION_ID, 
#'                min_ngram = 1, max_ngram = 2)
#' 
#' dim(dtm)
#' 
#' head(colnames(dtm))
#' 
#' head(rownames(dtm))
#' }
#' @export
Vec2Dtm <- function(doc_vec, docnames = names(doc_vec), min_ngram = 1, max_ngram = 1, 
                    remove_stopwords = TRUE, custom_stopwords = NULL, 
                    lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE,
                    stem_documents = FALSE, ...){
  
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
    stopwords <- c(stopwords, custom_stopwords)
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
      x[ ! x %in% stopwords ]
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
  
  dtm <- text2vec::create_dtm(itoken_src = it, 
                              vectorizer = vectorizer,
                              type = "dgCMatrix")
  
  rownames(dtm) <- docnames
  
  return(dtm)
}



