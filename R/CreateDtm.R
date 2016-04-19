#' @title Convert a character vector to a document term matrix.
#' @description This is the main document term matrix creating function for \code{textmineR}.
#' In most cases, all you need to do is import documents as a character vector in R and then 
#' run this function to get a document term matrix that is compatible with the 
#' rest of \code{textmineR}'s functionality and many other libraries. \code{CreateDtm}
#' is built on top of the excellent \code{\link[text2vec]{text2vec}} library.
#' 
#' @param doc_vec A character vector of documents. 
#' @param doc_names A vector of names for your documents. Defaults to 
#'        \code{names(doc_vec)}. If NULL, then doc_names is set to be 
#'        \code{1:length(doc_vec)}.
#' @param ngram_window A numeric vector of length 2. The first entry is the minimum
#'        n-gram size; the second entry is the maximum n-gram size. Defaults to
#'        \code{c(1, 1)}.
#' @param stopword_vec A character vector of stopwords you would like to remove.
#'        Defaults to \code{c(tm::stopwords("english"), tm::stopwords("SMART"))}. 
#'        If you do not want stopwords removed, specify \code{stopword_vec = c()}.
#' @param lower Do you want all words coerced to lower case? Defaults to \code{TRUE}
#' @param remove_punctuation Do you want to convert all non-alpha numeric 
#'        characters to spaces? Defaults to \code{TRUE}
#' @param remove_numbers Do you want to convert all numbers to spaces? Defaults 
#'        to \code{TRUE}
#' @param stem_lemma_function A function that you would like to apply to the 
#'        documents for stemming, lemmatization, or similar. See examples for
#'        usage.
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}.
#' @return A document term matrix of class \code{dgCMatrix}. The rows index 
#' documents. The columns index terms. The i, j entries represent the count of 
#' term j appearing in document i.
#' @note The following transformations are applied to \code{stopword_vec} as 
#'       well as \code{doc_vec}: 
#'       \code{lower}, 
#'       \code{remove_punctuation}, 
#'       \code{remove_numbers}
#'       
#'       See \code{\link[tm]{stopwords}} for details on the default to the 
#'       \code{stopword_vec} argument.
#' @examples
#' \dontrun{
#' data(nih_sample)
#' 
#' # DTM of unigrams and bigrams
#' dtm <- CreateDtm(doc_vec = nih_sample$ABSTRACT_TEXT,
#'                  doc_names = nih_sample$APPLICATION_ID, 
#'                  ngram_window = c(1, 2))
#' 
#' # DTM of unigrams with Porter's stemmer applied
#' dtm <- CreateDtm(doc_vec = nih_sample$ABSTRACT_TEXT,
#'                  doc_names = nih_sample$APPLICATION_ID,
#'                  stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"))
#' }
#' @export
CreateDtm <- function(doc_vec, doc_names = names(doc_vec), ngram_window = c(1, 1), 
                    stopword_vec = c(tm::stopwords("english"), tm::stopwords("SMART")), 
                    lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE,
                    stem_lemma_function = NULL, ...){
  
  ### Pre-process the documents ------------------------------------------------
  if(is.null(doc_names)){
    warning("No document names detected. Assigning 1:length(doc_vec) as names.")
    doc_names <- 1:length(doc_vec)
  }

  if (lower) {
    doc_vec <- tolower(doc_vec)
    stopword_vec <- tolower(stopword_vec)
  }
  
  if (remove_punctuation) {
    doc_vec <- stringr::str_replace_all(doc_vec, "[^a-zA-Z0-9]", " ")
    stopword_vec <- stringr::str_replace_all(stopword_vec, "[^a-zA-Z0-9]", " ")
    stopword_vec <- unique(unlist(stringr::str_split(string = stopword_vec, 
                                                  pattern = "\\s+")))
  }
  
  if (remove_numbers) {
    doc_vec <- stringr::str_replace_all(doc_vec, "[0-9]", " ")
    stopword_vec <- stringr::str_replace_all(stopword_vec, "[0-9]", " ")
    stopword_vec <- unique(unlist(stringr::str_split(string = stopword_vec, 
                                                  pattern = "\\s+")))
  }
  
  doc_vec <- stringr::str_replace_all(doc_vec, "\\s+", " ")
  stopword_vec <- stringr::str_replace_all(doc_vec, "\\s+", " ")
  
  ### Create iterators, vocabulary, other objects for dtm construction ---------
  
  # tokenize & construct vocabulary
  tokens <- text2vec::word_tokenizer(string = doc_vec)
  
  if(length(stopword_vec) > 0){
    # process in batches of 5,000

    batches <- seq(1, length(tokens), 5000)

    tokens <- lapply(batches, function(x) tokens[ x:min(x + 4999, length(tokens)) ])

    tokens <- textmineR::TmParallelApply(X = tokens, FUN = function(x){
      lapply(x, function(y) y[ ! y %in% stopword_vec ])
    }, export = "stopword_vec", ...)

    tokens <- do.call("c", tokens)
  }

  if(! is.null(stem_lemma_function)){
    tokens <- textmineR::TmParallelApply(X = tokens, FUN = stem_lemma_function, ...)
  }
  
  it <- text2vec::itoken(tokens)
  
  vocabulary <- text2vec::create_vocabulary(itoken_src = it, 
                                            ngram = ngram_window)
  
  
  vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
  
  ### Get the dtm, make sure it has correct dimnames, and return ---------------
  it <- text2vec::itoken(tokens)
  
  dtm <- text2vec::create_dtm(itoken_src = it, 
                              vectorizer = vectorizer,
                              type = "dgCMatrix")
  
  rownames(dtm) <- doc_names
  
  return(dtm)
}