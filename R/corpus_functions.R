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
#'        Defaults to \code{c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"))}. 
#'        If you do not want stopwords removed, specify \code{stopword_vec = c()}.
#' @param lower Do you want all words coerced to lower case? Defaults to \code{TRUE}
#' @param remove_punctuation Do you want to convert all non-alpha numeric 
#'        characters to spaces? Defaults to \code{TRUE}
#' @param remove_numbers Do you want to convert all numbers to spaces? Defaults 
#'        to \code{TRUE}
#' @param stem_lemma_function A function that you would like to apply to the 
#'        documents for stemming, lemmatization, or similar. See examples for
#'        usage.
#' @param verbose Defaults to \code{TRUE}. Do you want to see status during 
#'        vectorization?
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
                      stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart")), 
                      lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE,
                      stem_lemma_function = NULL, verbose = FALSE, ...){
  
  ### Pre-process the documents ------------------------------------------------
  if (is.null(doc_names) & is.null(names(doc_vec))) {
    warning("No document names detected. Assigning 1:length(doc_vec) as names.")
    doc_names <- 1:length(doc_vec)
  }
  
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
  
  it <- text2vec::itoken(tokens, progressbar = verbose)
  
  vocabulary <- text2vec::create_vocabulary(it = it, 
                                            ngram = ngram_window)
  
  
  vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
  
  ### Get the dtm, make sure it has correct dimnames, and return ---------------
  
  dtm <- text2vec::create_dtm(it = it, 
                              vectorizer = vectorizer,
                              verbose = verbose,
                              type = "dgCMatrix")
  
  rownames(dtm) <- doc_names
  
  # prepare attribute of arguments for repeating later
  attr(dtm, "args") <- list(
    doc_names = names(dtm),
    ngram_window = ngram_window, 
    stopword_vec = stopword_vec, 
    lower = lower, 
    remove_punctuation = remove_punctuation, 
    remove_numbers = remove_numbers,
    stem_lemma_function = stem_lemma_function, 
    verbose = verbose
  )
  
  attr(dtm, "call") <- "CreateDtm"

  return(dtm)
}

#' @title Convert a character vector to a term co-occurrence matrix.
#' @description This is the main term co-occurrence matrix creating function for \code{textmineR}.
#' In most cases, all you need to do is import documents as a character vector in R and then 
#' run this function to get a term co-occurrence matrix that is compatible with the 
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
#'        Defaults to \code{c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart"))}. 
#'        If you do not want stopwords removed, specify \code{stopword_vec = c()}.
#' @param lower Do you want all words coerced to lower case? Defaults to \code{TRUE}
#' @param remove_punctuation Do you want to convert all non-alpha numeric 
#'        characters to spaces? Defaults to \code{TRUE}
#' @param remove_numbers Do you want to convert all numbers to spaces? Defaults 
#'        to \code{TRUE}
#' @param stem_lemma_function A function that you would like to apply to the 
#'        documents for stemming, lemmatization, or similar. See examples for
#'        usage.
#' @param verbose Defaults to \code{TRUE}. Do you want to see status during 
#'        vectorization?
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}.
#' @return A document term matrix of class \code{dgCMatrix}. The rows index 
#' documents. The columns index terms. The i, j entries represent the count of 
#' term j appearing in document i.
#' @details Setting \code{skipgram_window} counts the number of times that term
#'          \code{j} appears within \code{skipgram_window} places of term \code{i}.
#'          \code{Inf} and \code{0} create somewhat special TCMs. Setting \code{skipgram_window}
#'          to \code{Inf} counts the number of documents in which term \code{j} 
#'          and term \code{i} occur together. Setting \code{skipgram_window}
#'          to \code{0} counts the number of terms shared by document \code{j} 
#'          and document \code{i}. A TCM where \code{skipgram_window} 
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
                      stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart")), 
                      lower = TRUE, remove_punctuation = TRUE, remove_numbers = TRUE,
                      stem_lemma_function = NULL, verbose = FALSE, ...){
  
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
  
  it <- text2vec::itoken(tokens, progressbar = verbose)
  
  vocabulary <- text2vec::create_vocabulary(it = it, 
                                            ngram = ngram_window)
  
  ### Get the tcm, make sure it has correct dimnames, and return ---------------
  
  if (is.infinite(skipgram_window)) { 
    
    vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
    
    dtm <- text2vec::create_dtm(it = it, 
                                vectorizer = vectorizer,
                                verbose = verbose,
                                type = "dgCMatrix")
    
    tcm <- textmineR::Dtm2Tcm(dtm = dtm)
    
  } else if (skipgram_window == 0) {
    
    vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
    
    dtm <- text2vec::create_dtm(it = it, 
                                vectorizer = vectorizer,
                                verbose = verbose,
                                type = "dgCMatrix")
    
    dtm <- dtm > 0
    
    tcm <- dtm %*% t(dtm)
    
  } else {
    
    vectorizer <- text2vec::vocab_vectorizer(vocabulary = vocabulary)
    
    
    tcm <- text2vec::create_tcm(it, vectorizer, 
                                skip_grams_window = as.integer(skipgram_window),
                                verbose = verbose)
    
    tcm <- methods::as(tcm, "dgCMatrix", strict = TRUE)
    
  }
  
  # prepare attribute of arguments for repeating later
  attr(tcm, "args") <- list(
    skipgram_window = skipgram_window, 
    ngram_window = ngram_window, 
    stopword_vec = stopword_vec, 
    lower = lower, 
    remove_punctuation = remove_punctuation, 
    remove_numbers = remove_numbers,
    stem_lemma_function = stem_lemma_function, 
    verbose = verbose
  )

  attr(tcm, "call") <- "CreateTcm"
  
  return(tcm)
}

#' Convert a DTM to a Character Vector of documents
#' 
#' @description This function takes a sparse matrix (DTM) as input and returns a character vector
#' whose length is equal to the number of rows of the input DTM.
#' @param dtm A sparse Matrix from the matrix package whose rownames correspond 
#' to documents and colnames correspond to words
#' @param ... Other arguments to be passed to \code{\link[textmineR]{TmParallelApply}}. See note, below.
#' @return
#' Returns a character vector. Each entry of this vector corresponds to the rows
#' of \code{dtm}.
#' @note
#' This function performs parallel computation if \code{dtm} has more than 3,000
#' rows. The default is to use all available cores according to \code{\link[parallel]{detectCores}}.
#' However, this can be modified by passing the \code{cpus} argument when calling
#' this function.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample)
#' data(nih_sample_dtm) 
#' 
#' # see the original documents
#' nih_sample$ABSTRACT_TEXT[ 1:3 ]
#' 
#' # see the new documents re-structured from the DTM
#' new_docs <- Dtm2Docs(dtm = nih_sample_dtm)
#' 
#' new_docs[ 1:3 ]
#' 
Dtm2Docs <- function(dtm, ...){
  
  # do in parallel in batches of about 3000 if we have more than 3000 docs
  if(nrow(dtm) > 3000){
    
    batches <- seq(1, nrow(dtm), by = 3000)
    
    dtm_list <- lapply(batches, function(x) dtm[ x:min(x + 2999, nrow(dtm)) , ])
    
    out <-TmParallelApply(X = dtm_list, FUN = function(x){
      Dtm2DocsC(dtm = x, vocab = colnames(x))
    }, ...)
    
  }else{
    out <- Dtm2DocsC(dtm = dtm, vocab = colnames(dtm))
  }
  
  out <- unlist(out)
  
  names(out) <- rownames(dtm)
  
  out
}

#' @title Turn a document term matrix into a term co-occurrence matrix
#' @description Turn a document term matrix, whose rows index documents and 
#' whose columns index terms, into a term co-occurrence matrix. A term co-occurrence
#' matrix's rows and columns both index terms. See \code{details}, below.
#' @param dtm A document term matrix, generally of class \code{dgCMatrix}, though
#' other classes, such as \code{dgTMatrix}, may also work without issue.
#' @return Returns a square \code{dgCMatrix} whose rows and columns both index
#' terms. The i, j entries of this matrix represent the count of term j across
#' documents containing term i. Note that, while square, this matrix is not
#' symmetric.
#' @examples
#' data(nih_sample_dtm)
#' 
#' tcm <- Dtm2Tcm(nih_sample_dtm)
#' @export
Dtm2Tcm <- function(dtm){
  
  # create a binary matrix
  dtm_binary <- dtm > 0
  
  # dot product gives us the result
  result <- Matrix::t(dtm_binary) %*% dtm
  
  result
}


#' @title Get term frequencies and document frequencies from a document term matrix.
#' @description This function takes a document term matrix as input and 
#' returns a data frame with columns for term frequency, document frequency, 
#' and inverse-document frequency
#' @param dtm A document term matrix of class \code{dgCMatrix}.
#' @return Returns a \code{data.frame} or \code{tibble} with 4 columns.
#' The first column, \code{term} is a vector of token labels.
#' The second column, \code{term_freq} is the count of times \code{term}
#' appears in the entire corpus. The third column \code{doc_freq} is the
#' count of the number of documents in which \code{term} appears.
#' The fourth column, \code{idf} is the log-weighted
#' inverse document frequency of \code{term}.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(nih_sample_dtm)
#' data(nih_sample_topic_model) 
#' 
#' # Get the term frequencies 
#' term_freq_mat <- TermDocFreq(nih_sample_dtm)
#' 
#' str(term_freq_mat)
TermDocFreq <- function(dtm){
  freq.mat <- data.frame(term=colnames(dtm), 
                         term_freq=Matrix::colSums(dtm), 
                         doc_freq=Matrix::colSums(dtm > 0), 
                         stringsAsFactors=FALSE)
  
  freq.mat$idf <- log(nrow(dtm) / freq.mat$doc_freq)

  if ("tibble" %in% row.names(utils::installed.packages())) {
      freq.mat <- tibble::as_tibble(freq.mat)
  }
  return(freq.mat)
}
