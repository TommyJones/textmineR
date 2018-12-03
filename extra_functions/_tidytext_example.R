################################################################################
# Example: Using tidytext with textmineR
################################################################################

library(tidytext)
library(textmineR)
library(dplyr)
library(tidyr)

# load documents in a data frame
docs <- textmineR::nih_sample 

# tokenize using tidytext's unnest_tokens
tidy_docs <- docs %>% 
  select(APPLICATION_ID, ABSTRACT_TEXT) %>% 
  unnest_tokens(output = word, 
                input = ABSTRACT_TEXT,
                stopwords = c(stopwords::stopwords("en"), 
                              stopwords::stopwords(source = "smart")),
                token = "ngrams",
                n_min = 1, n = 2) %>% 
  count(APPLICATION_ID, word) %>% 
  filter(n>1) #Filtering for words/bigrams per document, rather than per corpus

tidy_docs <- tidy_docs %>% # filter words that are just numbers
  filter(! stringr::str_detect(tidy_docs$word, "^[0-9]+$"))

# turn a tidy tbl into a sparse dgCMatrix for use in textmineR
d <- tidy_docs %>% 
  cast_sparse(APPLICATION_ID, word, n)


# create a topic model
m <- FitLdaModel(dtm = d, 
                 k = 20,
                 iterations = 200,
                 burnin = 175,
                 optimize_alpha = TRUE,
                 calc_likelihood = TRUE,
                 calc_r2 = TRUE)


# below is equivalent to tidy_beta <- tidy(x = m, matrix = "beta")
tidy_beta <- data.frame(topic = as.integer(stringr::str_replace_all(rownames(m$phi), "t_", "")), 
                        m$phi, 
                        stringsAsFactors = FALSE) %>%
  gather(term, beta, -topic) %>% 
  tibble::as.tibble()

# below is equivalent to tidy_gamma <- tidy(x = m, matrix = "gamma")
tidy_gamma <- data.frame(document = rownames(m$theta),
                         m$theta,
                         stringsAsFactors = FALSE) %>%
  gather(topic, gamma, -document) %>%
  tibble::as.tibble()
