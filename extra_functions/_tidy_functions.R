
# tidytext version of CreateDtm
create_dtm <- function (doc_vec, 
                        doc_names = names(doc_vec), 
                        ngram_window = c(1, 1), 
                        stopword_vec = tidytext::stop_words$word, 
                        lower = TRUE, 
                        remove_punctuation = TRUE, 
                        remove_numbers = TRUE, 
                        stem_lemma_function = NULL, 
                        verbose = FALSE, ...) {
  
  
  # check inputs ----
  if (verbose) 
    warning("verbose is deprecated and will be removed in a future version.")
  
  if (is.null(doc_names) & is.null(names(doc_vec))) {
    message("No document names detected. Assigning 1:length(doc_vec) as names.")
    doc_names <- 1:length(doc_vec)
  }

  # round the values of ngram_window in case someone is cheeky
  ngram_window <- ceiling(ngram_window)
  
  if (NA %in% ngram_window | min(ngram_window) < 1 | sum(is.infinite(ngram_window)) > 0) {
    stop("ngram_window cannot contain missing values, infinite values, or values less than 1")
  }

  # format using tidytext syntax ----
  tidy_docs <- tibble::data_frame(doc_names = doc_names,
                                  doc_vec = doc_vec)
  
  if (max(ngram_window) == 1) { # use word tokenizer
    tidy_docs <- tidy_docs %>%
      unnest_tokens(output = word,
                    input = doc_vec,
                    token = "words",
                    to_lower = lower,
                    strip_punct = remove_punctuation,
                    strip_numeric = remove_numbers)
    
  } else { # use ngram tokenizer
    tidy_docs <- tidy_docs %>%
      unnest_tokens(output = word,
                    input = doc_vec,
                    token = "ngrams",
                    to_lower = lower,
                    n_min = ngram_window[1],
                    n = ngram_window[2])
  }
  
  if (length(stopword_vec) > 0) {
    
    # this "if statement" has to be first to preserve proper stopword removal
    if (ngram_window[2] > 1) { # stopwords from ngram entries

      mega_regex <- paste0("(^| )(", paste(stopword_vec, collapse = "|"), ")( |$)")
      
      tidy_docs$word <- tidy_docs$word %>%
        stringr::str_remove_all(mega_regex) 
      
    }
    
    # this has to be second
    tidy_docs <- tidy_docs %>%
      anti_join(data_frame(word = stopword_vec), by = "word")
    
    
  }
  
  if (remove_numbers) { 
    # regex removes all strings of numbers even if they contain punctuation
    tidy_docs <- tidy_docs %>%
      filter(! stringr::str_detect(tidy_docs$word, "(^| )[0-9[:punct:]]+( |$)"))
    
    # removes any numeric characters in words that contain letters and numbers
    # EDIT: not doing this as many of these terms contain information e.g. "1980s"
    # tidy_docs$word <- tidy_docs$word %>%
    #   stringr::str_remove_all("[0-9]+")
  }
  
  if (remove_punctuation) {
    # removes any punctuation characters i
    tidy_docs$word <- tidy_docs$word %>%
      stringr::str_remove_all("[[:punct:]]+")
    
  }
  
  if (! is.null(stem_lemma_function)) {
    tidy_docs$word <- tidy_docs$word %>%
      stem_lemma_function()
  }
  
  # remove empty slot that may have appeard from above cleanup
  tidy_docs <- tidy_docs %>%
    anti_join(data_frame(word = c("", " ")), by = "word")
    
  # cast to document term matrix ----
  dtm <- tidy_docs %>%
    count(doc_names, word) %>%
    cast_sparse(doc_names, word, n)
  
  colnames(dtm) <- colnames(dtm) %>%
    stringr::str_replace_all(pattern = " +", "_")
  
  # return dtm
  dtm
  
}


# tidytext version of CreateTcm
create_tcm <- function(doc_vec, 
                       skipgram_window = Inf, 
                       ngram_window = c(1, 1), 
                       stopword_vec = tidytext::stop_words$word, 
                       lower = TRUE, 
                       remove_punctuation = TRUE, 
                       remove_numbers = TRUE, 
                       stem_lemma_function = NULL, 
                       verbose = FALSE, ...) {
  
  # check inputs ----
  if (verbose) 
    warning("verbose is deprecated and will be removed in a future version.")
  
  if (!is.numeric(skipgram_window)) {
    stop("skipgram_window must be a positive integer (including 0) or Inf")
  }
  
  if (!skipgram_window %in% c(Inf, 0)) {
    if (sum(ngram_window > 1) > 0) {
      stop("If skipgram_window is greater than 0 or non-infinite, ngram_window must be c(1, 1)")
    }
  }
  
  # round the values of ngram_window in case someone is cheeky
  ngram_window <- ceiling(ngram_window)
  
  # compute the TCM ----
  if (is.infinite(skipgram_window)) { # case 1: skipgram_window = Inf 
    d <- create_dtm(doc_vec = doc_vec,
                    doc_names = seq_along(doc_vec),
                    ngram_window = ngram_window,
                    stopword_vec = stopword_vec,
                    lower = lower,
                    remove_punctuation = remove_punctuation,
                    remove_numbers = remove_numbers,
                    stem_lemma_function = stem_lemma_function,
                    ...)
    
    d <- d > 0
    
    tcm <- t(d) %*% d
    
  } else if (skipgram_window == 0) { # case 2: skipgram_window = 0
    d <- create_dtm(doc_vec = doc_vec,
                    doc_names = seq_along(doc_vec),
                    ngram_window = ngram_window,
                    stopword_vec = stopword_vec,
                    lower = lower,
                    remove_punctuation = remove_punctuation,
                    remove_numbers = remove_numbers,
                    stem_lemma_function = stem_lemma_function,
                    ...)
    
    d <- d > 0
    
    tcm <- d %*% t(d)
    
  } else { # case 3: skipgram_window is something else
    
    # get data into a "tidy format"
    tidy_skipgrams <- tibble::data_frame(doc_names = seq_along(doc_vec),
                                         doc_vec = doc_vec)
    
    
    tidy_skipgrams <- tidy_skipgrams %>%
      unnest_tokens(output = ngram, # standard unnest
                    input = doc_vec,
                    token = "ngrams",
                    to_lower = lower,
                    n = skipgram_window) %>% 
      mutate(ngram_id = row_number()) %>% 
      unite(skipgram_id, doc_names, ngram_id) %>% 
      unnest_tokens(word, ngram,
                    token = "words",
                    to_lower = lower,
                    strip_punct = remove_punctuation,
                    strip_numeric = remove_numbers) 

    # make sure that removal of numbers, punctuation, etc. is handled the way I want
    if (length(stopword_vec) > 0) {
      tidy_skipgrams <- tidy_skipgrams %>%
        anti_join(data_frame(word = stopword_vec), by = "word")
    }
    
    if (remove_numbers) { 
      # regex removes all strings of numbers even if they contain punctuation
      tidy_skipgrams <- tidy_skipgrams %>%
        filter(! stringr::str_detect(tidy_skipgrams$word, "^[0-9[:punct:]]+$"))
      
      # removes any numeric characters in words that contain letters and numbers
      # EDIT: not doing this as many of these terms contain information e.g. "1980s"
      # tidy_skipgrams$word <- tidy_skipgrams$word %>%
      #   stringr::str_remove_all("[0-9]+")
    }
    
    if (remove_punctuation) {
      # removes any punctuation characters i
      tidy_skipgrams$word <- tidy_skipgrams$word %>%
        stringr::str_remove_all("[[:punct:]]+")
      
    }
    
    if (! is.null(stem_lemma_function)) {
      tidy_skipgrams$word <- tidy_skipgrams$word %>%
        stem_lemma_function()
    }
    
      
    # do pairwise counting
    tidy_skipgrams <- tidy_skipgrams %>%
      pairwise_count(word, skipgram_id, diag = TRUE, sort = TRUE) 
    
    # get the resulting TCM
    tcm <- tidy_skipgrams %>%
      cast_sparse(item1, item2, n)
    
  }
  

  # return out TCM ----
  tcm

}

# wrappers that fit with tidy modeling style guide
calc_gamma <- function(...) CalcGamma(...)

calc_hellinger_dist <- function(...) CalcHellingerDist(...)

calc_jsd <- function(...) CalcJSDivergence(...)

calc_likelihood <- function(...) CalcLikelihood(...)

calc_prob_coherence <- function(...) CalcProbCoherence(...)

calc_topic_model_r2 <- function(...) CalcTopicModelR2(...)

cluster_to_topic_model <- function(...) Cluster2TopicModel(...)

dtm_to_docs <- function(...) Dtm2Docs(...)

dtm_to_lexicon <- function(...) Dtm2Lexicon(...)

dtm_to_tcm <- function(...) Dtm2Tcm(...)

fit_ctm_model <- function(...) FitCtmModel(...)

fit_lda_model <- function(...) FitLdaModel(...)

fit_lsa_model <- function(...) FitLsaModel(...)

get_probable_terms <- function(...) GetProbableTerms(...)

get_top_terms <- function(...) GetTopTerms(...)

label_topics <- function(...) LabelTopics(...)

summarize_topics <- function(...) SummarizeTopics(...)

term_doc_freq <- function(...) TermDocFreq(...)


