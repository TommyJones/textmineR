
# Make sure distance functions don't overwrite inputs
testthat::test_that("distnace functions don't overwrite inputs",{
  
  data("AssociatedPress", package = "topicmodels")
  
  lda_owl <- topicmodels::LDA(AssociatedPress, k = 3, method = "Gibbs",
                              control = list(verbose = 50L, iter = 500, seed = 123))
  
  # Hellinger
  beta1 <- lda_owl@beta
  
  tidybeta1 <- tidytext::tidy(lda_owl, matrix = "beta")
  
  dist_owl <- textmineR::CalcHellingerDist(lda_owl@beta)
  
  beta2 <- lda_owl@beta
  
  tidybeta2 <- tidytext::tidy(lda_owl, matrix = "beta")
  
  testthat::expect_equal(sum(beta1 != beta2), 0) 
  
  testthat::expect_equal(sum(tidybeta1[[3]] != tidybeta2[[3]]), 0) 
  
  
  # JSD
  beta1 <- lda_owl@beta
  
  tidybeta1 <- tidytext::tidy(lda_owl, matrix = "beta")
  
  dist_owl <- textmineR::CalcJSDivergence(lda_owl@beta)
  
  beta2 <- lda_owl@beta
  
  tidybeta2 <- tidytext::tidy(lda_owl, matrix = "beta")
  
  testthat::expect_equal(sum(beta1 != beta2), 0) 
  
  testthat::expect_equal(sum(tidybeta1[[3]] != tidybeta2[[3]]), 0) 
  
})