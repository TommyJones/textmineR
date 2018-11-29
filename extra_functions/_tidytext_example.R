################################################################################
# Example: Using tidytext with textmineR
################################################################################

library(tidytext)
library(textmineR)

# load documents in a data frame
docs <- textmineR::nih_sample

# tokenize using tidytext's unnest_tokens
# using arguments similar to what we'd get from CreateDtm
tidy_docs <- unnest_tokens(tbl = docs[,c("APPLICATION_ID", "ABSTRACT_TEXT")], 
                           output = word, 
                           input = ABSTRACT_TEXT,
                           stopwords = c(stopwords::stopwords("en"), 
                                         stopwords::stopwords(source = "smart")),
                           token = "ngrams",
                           n_min = 1, n = 2) 

# turn a tidy tbl into a sparse dgCMatrix for use in textmineR
d <- cast_sparse(data = cbind(tidy_docs, count = 1), 
                 row = APPLICATION_ID, column = word, value = count)

# remove the lowest-frequency words before modeling
d <- d[,colSums(d) > 1]


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
  reshape2::melt(id.vars = "topic", 
             variable.name = "term",
             value.name = "beta") %>%
  tibble::as.tibble()

tidy_beta$term <- as.character(tidy_beta$term)

tidy_beta

# below is equivalent to tidy_gamma <- tidy(x = m, matrix = "gamma")
tidy_gamma <- data.frame(document = rownames(m$theta),
                         m$theta,
                         stringsAsFactors = FALSE) %>%
  reshape2::melt(id.vars = "document", 
                 variable.name = "topic",
                 value.name = "gamma") %>%
  tibble::as.tibble()

tidy_gamma$topic <- as.character(tidy_gamma$topic) %>%
  stringr::str_replace_all("t_", "") %>%
  as.integer()

tidy_gamma
