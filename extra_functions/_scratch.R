library(tidytext)
library(textmineR)
library(dplyr)
library(tidyr)
library(widyr)

# load documents in a data frame
docs <- textmineR::nih_sample 

# tokenize using tidytext's unnest_tokens, unigrams only
tidy_docs <- docs %>%
  select(APPLICATION_ID, ABSTRACT_TEXT) %>%
  unnest_tokens(output = word,
                input = ABSTRACT_TEXT,
                token = "words",
                strip_numeric = TRUE) %>%
  anti_join(stop_words, by = "word")

### DTM ----

d <- tidy_docs %>% 
  count(APPLICATION_ID, word) %>%
  filter(n>1) %>% #Filtering for words/bigrams per document, rather than per corpus
  cast_sparse(APPLICATION_ID, word, n) # turn a tidy tbl into a sparse dgCMatrix for use in textmineR

### TCM all ----




### TCM skipgram ----
tidy_skipgrams <- docs %>%
  select(APPLICATION_ID, ABSTRACT_TEXT) %>% # get the right columns
  unnest_tokens(output = ngram, # standard unnest
                input = ABSTRACT_TEXT,
                token = "ngrams",
                n = 5) %>% # set skipgram window here
  mutate(ngram_id = row_number()) %>% # add a column called ngram_id that is 1:nrow(tidy_skipgrams)
  unite(skipgram_id, APPLICATION_ID, ngram_id) %>% # paste together APPLICATION_ID and ngram_id into a new column, skipgram_id. remove other columns
  unnest_tokens(word, ngram) %>% # ngrams are now documents. tokenize into unigrams
  pairwise_count(word, skipgram_id, diag = TRUE, sort = TRUE) # count pairs of words that co-occur

# let's remove stopwords here
tidy_skipgrams <- tidy_skipgrams %>%
  filter(! tidy_skipgrams$item1 %in% stop_words$word &
           ! tidy_skipgrams$item2 %in% stop_words$word)

tidy_skipgrams <- tidy_skipgrams %>%
  filter(! stringr::str_detect(tidy_skipgrams$item1, "^[0-9,\\.]+$") &
           ! stringr::str_detect(tidy_skipgrams$item2, "^[0-9,\\.]+$"))

t_skip <- tidy_skipgrams %>% 
  filter(n > 1) %>%
  cast_sparse(item1, item2, n)


# playing around with model. will delete later
m <- FitLdaModel(dtm = t_skip,
                 k = 100,
                 iterations = 300,
                 burnin = 275,
                 beta = colSums(t_skip) / sum(t_skip) * (0.05 * ncol(t_skip)),
                 optimize_alpha = TRUE, calc_likelihood = TRUE, calc_r2 = TRUE)
m$r2

summary(m$coherence)

plot(m$log_likelihood, type = "o")

h <- (CalcHellingerDist(m$phi) + CalcHellingerDist(t(m$theta))) / 2

plot(hclust(as.dist(h), "ward.D"), labels = apply(GetTopTerms(m$phi, 3),2,paste, collapse = ", "))


### TCM leading skipgram ----


### TCM trailing skipgram ----





