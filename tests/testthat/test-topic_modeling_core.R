context("Topic Modeling Core")


### CalcGamma ----
test_that("CalcGamma works as expected",{
  
  g <- CalcGamma(phi = nih_sample_topic_model$phi,
                 theta = nih_sample_topic_model$theta)
  
  
  # check dimensions
  expect_true(nrow(g) == nrow(nih_sample_topic_model$phi))
  
  expect_true(ncol(g) == ncol(nih_sample_topic_model$phi))
  
  # check_sums
  expect_true(mean(round(colSums(g),10)) == 1) # round b/c numeric precision
  
  
})

### Cluster2TopicModel ----
test_that("Cluster2TopicModel works as expected",{
  cl <- Cluster2TopicModel(nih_sample_dtm, nih_sample$ADMINISTERING_IC)
  
  # check dimensions
  expect_true(nrow(cl$theta) == nrow(nih_sample_dtm))
  
  expect_true(ncol(cl$theta) == length(unique(nih_sample$ADMINISTERING_IC)))
  
  expect_true(nrow(cl$phi) == ncol(cl$theta))
  
  expect_true(ncol(cl$phi) == ncol(nih_sample_dtm))
  
  expect_true(nrow(cl$gamma) == nrow(cl$phi))
  
  expect_true(ncol(cl$gamma) == ncol(cl$phi))
  
  # check sums (all key statistics from summary of sums are 1)
  expect_true(sum(summary(round(rowSums(cl$theta)),10)) == 6)
  
  expect_true(sum(summary(round(rowSums(cl$phi)),10)) == 6)
  
  expect_true(sum(summary(round(colSums(cl$gamma)),10)) == 6)
  
})

### FitCtmModel ----
test_that("FitCtmModel performs as expected",{
  docs <- c("This is my first document.",
            "My 2nd document!",
            "skills, son, skills. Skillz!")
  
  d <- CreateDtm(doc_vec = docs, doc_names = seq_along(docs),
                 ngram_window = c(1,2),
                 stopword_vec = "the", 
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 cpus = 2)
  
  m <- FitCtmModel(dtm = d, k = 2, calc_coherence = FALSE, calc_r2 = FALSE)
  
  # check dimensions
  expect_true(nrow(m$theta) == nrow(d))
  
  expect_true(ncol(m$theta) == 2)
  
  expect_true(nrow(m$phi) == ncol(m$theta))
  
  expect_true(ncol(m$phi) == ncol(d))
  
  expect_true(nrow(m$gamma) == nrow(m$phi))
  
  expect_true(ncol(m$gamma) == ncol(m$phi))
  
  # check sums (all key statistics from summary of sums are 1)
  expect_true(sum(summary(round(rowSums(m$theta)),10)) == 6)
  
  expect_true(sum(summary(round(rowSums(m$phi)),10)) == 6)
  
  expect_true(sum(summary(round(colSums(m$gamma)),10)) == 6)
  
  
})

### predict.ctm_topic_model ----
test_that("predict.ctm_topic_model performs as expected", {
  docs <- c("This is my first document.",
            "My 2nd document!",
            "skills, son, skills. Skillz!")
  
  d <- CreateDtm(doc_vec = docs, doc_names = seq_along(docs),
                 ngram_window = c(1,2),
                 stopword_vec = "the", 
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 cpus = 2)
  
  m <- FitCtmModel(dtm = d, k = 2, calc_coherence = FALSE, calc_r2 = FALSE)
  
  # predict with a bunch of documents
  p <- predict(m, d)
  
  expect_true(nrow(p) == nrow(d))
  
  expect_true(ncol(p) == ncol(m$theta))
  
  expect_true(round(mean(rowSums(p)),10) == 1)
  
  # predict with one document
  p <- predict(m, d[1,])
  
  expect_true(nrow(p) == 1)
  
  expect_true(ncol(p) == ncol(m$theta))
  
  expect_true(round(sum(p),10) == 1)
  
})

### FitLsaModel ----


### predict.lsa_topic_model ----


### Dtm2Lexicon ----


### FitLdaModel ----


### predict.lda_topic_model ----









