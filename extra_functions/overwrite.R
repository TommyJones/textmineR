library(topicmodels)
data("AssociatedPress")

lda_owl <- topicmodels::LDA(AssociatedPress, k = 3, method = "Gibbs",
                            control = list(verbose = 50L, iter = 500, seed = 123))

tidybeta1 <- tidytext::tidy(lda_owl, matrix = "beta")

dist_owl <- textmineR::CalcHellingerDist(lda_owl@beta)

tidybeta2 <- tidytext::tidy(lda_owl, matrix = "beta")

sum(tidybeta1[[3]] != tidybeta2[[3]])



m <- textmineR::nih_sample_topic_model

beta1 <- tidylda::tidy(m$phi, "beta")

h <- textmineR::CalcHellingerDist(m$phi)

beta2 <- tidylda::tidy(m$phi, "beta")

sum(beta1[[3]] != beta2[[3]])



library(topicmodels)
data("AssociatedPress")

lda_owl <- topicmodels::LDA(AssociatedPress, k = 3, method = "Gibbs",
                            control = list(verbose = 50L, iter = 500, seed = 123))

beta1 <- lda_owl@beta

tidybeta1 <- tidytext::tidy(lda_owl, matrix = "beta")

dist_owl <- textmineR::CalcHellingerDist(lda_owl@beta)

beta2 <- lda_owl@beta

tidybeta2 <- tidytext::tidy(lda_owl, matrix = "beta")

testthat::expect_equal(sum(beta1 != beta2), 0) # no error

testthat::expect_equal(sum(tidybeta1[[3]] != tidybeta2[[3]]), 0) # error
