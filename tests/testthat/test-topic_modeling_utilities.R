context("topic_modeling_utilities")

### GetTopTerms ----
test_that("GetTopTerms performs as expected",{
  
  t <- GetTopTerms(nih_sample_topic_model$phi, M = 5)
  
  expect_true(nrow(t) == 5)
  
  expect_true(ncol(t) == nrow(nih_sample_topic_model$phi))

})


### GetProbableTerms ----

# check some errors here
# what happens with topics that get no assignment?


### LabelTopics ----


### SummarizeTopics ----



