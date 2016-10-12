#' @title Convert a character vector to a term co-occurence matrix.
#' @description This is the main term co-occurence matrix creating function for \code{textmineR}.
#' In most cases, all you need to do is import documents as a character vector in R and then 
#' run this function to get a term co-occurence matrix that is compatible with the 
#' rest of \code{textmineR}'s functionality and many other libraries. \code{CreateTcm}
#' is built on top of the excellent \code{\link[text2vec]{text2vec}} library.
#' 
#' @param doc_vec A character vector of documents. 
#' @param skipgram_window An integer window, from \code{0} to \code{Inf} for 
#'        skip-grams. Defaults to \code{Inf}. See 'Details', below.
#' @param ngram_window A numeric vector of length 2. The first entry is the minimum
#'        n-gram size; the second entry is the maximum n-gram size. Defaults to
#'        \code{c(1, 1)}. Must be \code{c(1, 1)} if \code{skipgram_window} is 
#'        not \code{0} or \code{Inf}.
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
#' @details Setting \code{skipgram_window} counts the number of times that term
#'          \code{j} appears within \code{skipgram_window} places of term \code{i}.
#'          \code{Inf} and \code{0} create somewhat special TCMs. Setting \code{skipgram_window}
#'          to \code{Inf} counts the number of times that term \code{j} appears 
#'          across all documents containing \code{i}. Setting \code{skipgram_window}
#'          to \code{0} counts the number of documents in which term \code{j} 
#'          and term \code{i} occur together. A TCM where \code{skipgram_window} 
#'          is \code{0} is the only TCM that will be symmetric.
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
#' # TCM of unigrams and bigrams
#' tcm <- CreateTcm(doc_vec = nih_sample$ABSTRACT_TEXT,
#'                  skipgram_window = Inf, 
#'                  ngram_window = c(1, 2))
#' 
#' # TCM of unigrams and a skip=gram window of 3, applying Porter's word stemmer
#' tcm <- CreateTcm(doc_vec = nih_sample$ABSTRACT_TEXT,
#'                  skipgram_window = 3,
#'                  stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"))
#' }
#' @export
CreateTcm <- function(doc_vec, skipgram_window = Inf, ngram_window = c(1, 1), 
                      stopword_vec = c(tm::stopwords("english"), tm::stopwords("SMART")), 
                      lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE,
                      stem_lemma_function = NULL, ...){
  
  ### Check inputs -------------------------------------------------------------
  if (! is.numeric(skipgram_window)) {
    stop("skipgram_window must be a positive integer (including 0) or Inf")
  }
  
  if (! skipgram_window %in% c(Inf, 0)) {
    if (sum(ngram_window > 1) >0) {
      stop("If skipgram_window is greater than 0 or non-infinite, ngram_window must be c(1, 1)")
    }
  }
  
  ### Pre-process the documents ------------------------------------------------
  if (lower) {
    doc_vec <- tolower(doc_vec)
    stopword_vec <- tolower(stopword_vec)
  }
  
  if (remove_punctuation) {
    doc_vec <- stringr::str_replace_all(doc_vec, "[^[:alnum:]]", " ")
    stopword_vec <- stringr::str_replace_all(stopword_vec, "[^[:alnum:]]", " ")
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
  stopword_vec <- stringr::str_replace_all(stopword_vec, "\\s+", " ")
  
  ### Create iterators, vocabulary, other objects for dtm construction ---------
  
  # tokenize & construct vocabulary
  tokens <- text2vec::word_tokenizer(string = doc_vec)
  
  if (length(stopword_vec) > 0) {
    # process in batches of 5,000
    
    batches <- seq(1, length(tokens), 5000)
    
    tokens <- lapply(batches, function(x) tokens[ x:min(x + 4999, length(tokens)) ])
    
    tokens <- textmineR::TmParallelApply(X = tokens, FUN = function(x){
      lapply(x, function(y) y[ ! y %in% stopword_vec ])
    }, export = "stopword_vec", ...)
    
    tokens <- do.call("c", tokens)
  }
  
  if (! is.null(stem_lemma_function)) {
    tokens <- textmineR::TmParallelApply(X = tokens, FUN = stem_lemma_function, ...)
  }
  
  tokens <- textmineR::TmParallelApply(X = tokens, 
                                       FUN = function(x) paste(x, collapse = " "),
                                       ...)
  
  tokens <- unlist(tokens)
  
  it <- text2vec::itoken(tokens)
  
  vocabulary <- text2vec::create_vocabulary(it = it, 
                                            ngram = ngram_window)
  
  ### Get the tcm, make sure it has correct dimnames, and return ---------------
  
  if (is.infinite(skipgram_window)) { 
    
    vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
    
    dtm <- text2vec::create_dtm(it = it, 
                                vectorizer = vectorizer,
                                type = "dgCMatrix")
    
    return(textmineR::Dtm2Tcm(dtm = dtm))
    
  } else if (skipgram_window == 0) {
    
    vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
    
    dtm <- text2vec::create_dtm(it = it, 
                                vectorizer = vectorizer,
                                type = "dgCMatrix")
    
    dtm <- dtm > 0
    
    return(dtm %*% t(dtm))
  } else {
    
    vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary,
                                             grow_dtm = FALSE,
                                             skip_grams_window = as.integer(skipgram_window))
    
    
    tcm <- text2vec::create_tcm(it, vectorizer)
    
    tcm <- methods::as(tcm, "dgCMatrix", strict = TRUE)
    
    return(tcm)
  }
}