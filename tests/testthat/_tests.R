################################################################################
# This script is for tests of textmineR
################################################################################

context("Get started with testing")

# library(textmineR)
# 
# library(testthat)
# 
# # source('~/Documents/___R_Development/textmineR/R/topic_modeling_core.R', echo=TRUE)
# # source('~/Documents/___R_Development/textmineR/R/corpus_functions.R', echo=TRUE)
# # source('~/Documents/___R_Development/textmineR/R/topic_modeling_utilities.R', echo=TRUE)
# # Rcpp::sourceCpp('src/lda_c_functions.cpp')

set.seed(12345)

d <- textmineR::nih_sample


################################################################################
# for now just a bunch of random crap


### Corpus functions ----
dtm <- CreateDtm(doc_vec = d$ABSTRACT_TEXT, 
                 doc_names = d$APPLICATION_ID,
                 ngram_window = c(1,2))

dtm <- dtm[,colSums(dtm) > 2]

tcm <- CreateTcm(doc_vec = d$ABSTRACT_TEXT,
                 skipgram_window = 10)

docs <- Dtm2Docs(dtm)

tcm2 <- Dtm2Tcm(dtm)

tdf <- TermDocFreq(dtm)

### Topic modeling core ----

# fit lsa models and predict
lsa <- FitLsaModel(dtm, 10)

lsa_e <- FitLsaModel(tcm, 20)

expect_error(
  plsa1 <- predict(lsa, d$PROJECT_TITLE)
)

plsa2 <- predict(lsa, CreateDtm(d$PROJECT_TITLE,
                                doc_names = d$APPLICATION_ID,
                                ngram_window = c(1,2)))

expect_error(
  plsa3 <- predict(lsa, d$PROJECT_TITLE[1]) 
)

plsa4 <- predict(lsa, CreateDtm(d$PROJECT_TITLE,
                                doc_names = d$APPLICATION_ID,
                                ngram_window = c(1,2))[1,])

plsa_e1 <- predict(lsa_e, d$PROJECT_TITLE)

plsa_e2 <- predict(lsa_e, CreateTcm(d$PROJECT_TITLE, skipgram_window = 10))

# fit lda models and predict

lda <- FitLdaModel(dtm = dtm, k = 10, # also add checks for missing arguments
                   iterations = 500, burnin = 450,
                   alpha = 0.05,
                   beta = colSums(dtm) / sum(colSums(dtm)) * 200,
                   optimize_alpha = TRUE,
                   calc_likelihood = TRUE, # you need to remove one of the columns of the output
                   calc_coherence = TRUE,
                   calc_r2 = TRUE)

lda_e <- FitLdaModel(dtm = tcm,
                     k = 20,
                     iterations = 500, burnin = 450,
                     alpha = 0.05,
                     beta = colSums(tcm) / sum(colSums(tcm)) * 100,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE, # you need to remove one of the columns of the output
                     calc_coherence = TRUE,
                     calc_r2 = TRUE)

expect_error(
  plda1 <- predict(lda, d$PROJECT_TITLE, 
                   method = "gibbs", 
                   iterations = 500, 
                   burnin = 450)
)

plda2 <- predict(lda, CreateDtm(d$PROJECT_TITLE,
                                doc_names = d$APPLICATION_ID,
                                ngram_window = c(1,2)), 
                 method = "gibbs", 
                 iterations = 500, 
                 burnin = 450)

expect_error(
  plda3 <- predict(lda, d$PROJECT_TITLE[1], 
                   method = "gibbs", 
                   iterations = 500, 
                   burnin = 450)
)

plda4 <- predict(lda, CreateDtm(d$PROJECT_TITLE,
                                doc_names = d$APPLICATION_ID,
                                ngram_window = c(1,2))[1,], 
                 method = "gibbs", 
                 iterations = 500, 
                 burnin = 450)

plda_e1 <- predict(lda_e, d$ABSTRACT_TEXT, # turns out if it's too sparse it bonks...
                   method = "gibbs", 
                   iterations = 500, 
                   burnin = 450)

plda_e2 <- predict(lda_e, d$PROJECT_TITLE, # turns out if it's too sparse it bonks on gibbs...
                   method = "dot")

plda_e3 <- predict(lda_e, CreateTcm(d$ABSTRACT_TEXT,
                                    skipgram_window = 10)[1:10,], # turns out if it's too sparse it bonks...
                   method = "gibbs", 
                   iterations = 500, 
                   burnin = 450)



