context("Functions related to corpus management")

# declare some globals
docs <- c("This is my first document.",
          "My 2nd document!",
          "skills, son, skills. Skillz!")



### CreateDtm ----
test_that("CreateDtm performs as expected",{

  d <- CreateDtm(doc_vec = docs, doc_names = seq_along(docs),
                 ngram_window = c(1,2),
                 stopword_vec = "the", 
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 cpus = 2)
  
  # all documents accounted for?
  expect_equal(length(docs), nrow(d))
  
  # stopwords removed?
  expect_false("the" %in% colnames(d))
  
  # correct number of unigrams and bigrams?
  expect_true(sum(! grepl("_", colnames(d))) == 9)
  
  # lowercase?
  expect_true(sum(grepl("[A-Z]", colnames(d))) == 0)
  
  # punctuation removed?
  expect_true(sum(grepl("[^[:alnum:]_]", colnames(d))) == 0)
  
  # numbers removed?
  expect_true(sum(grepl("[0-9]", colnames(d))) == 0)
  
})



### CreateTcm ----
test_that("CreateTcm performs as expected",{
  
  d <- CreateTcm(doc_vec = docs, 
                 skipgram_window = 3,
                 stopword_vec = "the", 
                 lower = TRUE,
                 remove_punctuation = TRUE,
                 remove_numbers = TRUE,
                 cpus = 2)

  # stopwords removed?
  expect_false("the" %in% colnames(d))
  
  # lowercase?
  expect_true(sum(grepl("[A-Z]", colnames(d))) == 0)
  
  # punctuation removed?
  expect_true(sum(grepl("[^[:alnum:]_]", colnames(d))) == 0)
  
  # numbers removed?
  expect_true(sum(grepl("[0-9]", colnames(d))) == 0)
})


### Dtm2Docs ----



### Dtm2Tcm ----



### TermDocFreq ----
