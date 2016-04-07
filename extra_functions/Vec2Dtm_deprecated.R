#' @title Convert a character vector to a document term matrix of class Matrix.
#' @description This function is deprecated. Use \code{\link[textmineR]{CreateDtm}}
#'              instead.
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
#' @export
Vec2Dtm <- function(doc_vec, docnames = names(doc_vec), min_ngram = 1, max_ngram = 1, 
                    remove_stopwords = TRUE, custom_stopwords = NULL, 
                    lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE,
                    stem_documents = FALSE, ...){
  
  .Deprecated(new = "CreateDtm", package = "textmineR",
              msg = "Vec2Dtm is deprecated and will be removed in textmineR v3.0
              Use 'CreateDtm' instead.",
              old = "Vec2Dtm")
  
  # Recast Vec2Dtm inputs as CreateDtm inputs
  ngram_window <- c(min_ngram, max_ngram)
  
  stopword_vec <- c()
  
  stopword_vec <- c(stopword_vec, custom_stopwords)
  
  if(remove_stopwords){
    stopwords_vec <- c(stopword_vec, tm::stopwords("english"), tm::stopwords("SMART"))
  }
  
  if(stem_documents){
    stem_lemma_function <- function(x) SnowballC::wordStem(x, "porter")
  }else{
    stem_lemma_functions <- NULL
  }
  
  # Use CreateDtm to get our result
  result <- textmineR::CreateDtm(doc_vec = doc_vec, docnames = docnames,
                                 ngram_window = ngram_window,
                                 stopword_vec = stopword_vec, lower = lower, 
                                 remove_punctuation = remove_punctuation,
                                 remove_numbers = remove_numbers,
                                 stem_lemma_function = stem_lemma_function, ...)
  
  result
  
}

