#' @title Convert a character vector to a document term matrix of class Matrix.
#' @description This function is deprecated. Use \code{\link[textmineR]{CreateDtm}}
#'              instead.
#' @param vec A character vector of documents. 
#' @param docnames A vector of names for your documents. Defaults to 
#'        \code{names(doc_vec)}. If NULL, then docnames is set to be 
#'        \code{1:length(doc_vec)}.
#' @param min.n.gram The minimum size of n for creating n-grams. Defaults to 1.
#' @param max.n.gram The maximum size of n for creating n-grams. Defaults to 1. 
#'        Numbers greater than 3 are discouraged due to risk of overfitting.
#' @param remove.stopwords Do you want to remove standard stopwords from your documents? 
#'        Defaults to \code{TRUE}.
#' @param custom.stopwords If not \code{NULL} (the default) a character vector 
#'        of stopwords to remove from your corpus. 
#' @param lower Do you want all words coerced to lower case? Defaults to \code{TRUE}
#' @param remove.punctuation Do you want to convert all non-alpha numeric 
#'        characters to spaces? Defaults to \code{TRUE}
#' @param remove.numbers Do you want to convert all numbers to spaces? Defaults 
#'        to \code{TRUE}
#' @param stem.document Do you want to stem the words in your document using 
#'        Porter's word stemmer? Defaults to \code{FALSE}
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}.
#' @return A document term matrix of class \code{dgCMatrix}. The rows index 
#' documents. The columns index terms. The i, j entries represent the count of 
#' term j appearing in document i.
#' @examples
#' \dontrun{
#' data(nih_sample)
#' 
#' 
#' dtm <- Vec2Dtm(vec = nih_sample$ABSTRACT_TEXT,
#'                docnames = nih_sample$APPLICATION_ID, 
#'                min.n.gram = 1, max.n.gram = 2)
#' 
#' dim(dtm)
#' 
#' head(colnames(dtm))
#' 
#' head(rownames(dtm))
#' }
#' @export
Vec2Dtm <- function(vec, docnames = names(vec), min.n.gram=1, max.n.gram=1, 
                    remove.stopwords=TRUE, custom.stopwords=NULL, lower=TRUE, 
                    remove.punctuation=TRUE, remove.numbers=TRUE, stem.document=FALSE,
                    ...){
  
  .Deprecated(new = "CreateDtm", package = "textmineR",
              msg = "Vec2Dtm is deprecated and will be removed in textmineR v3.0
              Use 'CreateDtm' instead.",
              old = "Vec2Dtm")
  
  # Recast Vec2Dtm inputs as CreateDtm inputs
  ngram_window <- c(min.n.gram, max.n.gram)
  
  stopword_vec <- c()
  
  stopword_vec <- c(stopword_vec, custom.stopwords)
  
  if(remove.stopwords){
    stopwords_vec <- c(stopword_vec, tm::stopwords("english"), tm::stopwords("SMART"))
  }
  
  if(stem.document){
    stem_lemma_function <- function(x) SnowballC::wordStem(x, "porter")
  }else{
    stem_lemma_functions <- NULL
  }
  
  # Use CreateDtm to get our result
  result <- textmineR::CreateDtm(doc_vec = vec, doc_names = docnames,
                                 ngram_window = ngram_window,
                                 stopword_vec = stopword_vec, lower = lower, 
                                 remove_punctuation = remove.punctuation,
                                 remove_numbers = remove.numbers,
                                 stem_lemma_function = stem_lemma_function, ...)
  
  result
  
}

