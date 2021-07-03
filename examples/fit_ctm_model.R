# Load a pre-formatted dtm
data(nih_sample_dtm)

# Fit a CTM model on a sample of documents
model <- FitCtmModel(
  dtm = nih_sample_dtm[sample(1:nrow(nih_sample_dtm), 10), ],
  k = 3, return_all = FALSE
)

# the correct way to pass control arguments to CTM
\dontrun{
topics_CTM <- FitCtmModel(
  dtm = nih_sample_dtm[sample(1:nrow(nih_sample_dtm), 10), ],
  k = 10,
  calc_coherence = TRUE,
  calc_r2 = TRUE,
  return_all = TRUE,
  estimate.beta = TRUE,
  verbose = 0,
  prefix = tempfile(),
  save = 0,
  keep = 0,
  seed = as.integer(Sys.time()),
  nstart = 1L,
  best = TRUE,
  var = list(iter.max = 500, tol = 10^-6),
  em = list(iter.max = 1000, tol = 10^-4),
  initialize = "random",
  cg = list(iter.max = 500, tol = 10^-5)
)
}
