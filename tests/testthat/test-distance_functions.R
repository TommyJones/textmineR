
# Make sure distance functions don't overwrite inputs
testthat::test_that("distnace functions don't overwrite inputs",{
  
  tidy_fun <- function(x) {
    data.frame(
      topic = as.integer(stringr::str_replace_all(rownames(x), "t_", "")), 
      m$phi,
      stringsAsFactors = FALSE
    )
  }

  
  # Hellinger
  m <- nih_sample_topic_model

    beta1 <- m$phi
  
  tidybeta1 <- tidy_fun(m$phi)
  
  dist_owl <- CalcHellingerDist(m$phi)
  
  beta2 <- m$phi
  
  tidybeta2 <- tidy_fun(m$phi)
  
  testthat::expect_equal(sum(beta1 != beta2), 0) 
  
  testthat::expect_equal(sum(tidybeta1[[3]] != tidybeta2[[3]]), 0) 
  
  
  # JSD
  m <- nih_sample_topic_model
  
  beta1 <- m$phi
  
  tidybeta1 <- tidy_fun(m$phi)
  
  dist_owl <- CalcJSDivergence(m$phi)
  
  beta2 <- m$phi
  
  tidybeta2 <- tidy_fun(m$phi)
  
  testthat::expect_equal(sum(beta1 != beta2), 0) 
  
  testthat::expect_equal(sum(tidybeta1[[3]] != tidybeta2[[3]]), 0) 
  
})