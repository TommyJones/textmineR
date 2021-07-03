\dontrun{
# load a document term matrix
d1 <- nih_sample_dtm[1:50, ]

d2 <- nih_sample_dtm[51:100, ]

# fit a model
m <- FitLdaModel(d1,
  k = 10,
  iterations = 200, burnin = 175,
  optimize_alpha = TRUE,
  calc_likelihood = FALSE,
  calc_coherence = TRUE,
  calc_r2 = FALSE
)

# update an existing model by adding documents
m2 <- update(
  object = m,
  dtm = rbind(d1, d2),
  iterations = 200,
  burnin = 175
)

# use an old model as a prior for a new model
m3 <- update(
  object = m,
  dtm = d2, # new documents only
  iterations = 200,
  burnin = 175
)

# add topics while updating a model by adding documents
m4 <- update(
  object = m,
  dtm = rbind(d1, d2),
  additional_k = 3,
  iterations = 200,
  burnin = 175
)

# add topics to an existing model
m5 <- update(
  object = m,
  dtm = d1, # this is the old data
  additional_k = 3,
  iterations = 200,
  burnin = 175
)
}
