# Load a pre-formatted dtm and topic model
data(nih_sample_dtm)
data(nih_sample_topic_model)

# Get the likelihood of the data given the fitted model parameters
ll <- calc_likelihood(
  dtm = nih_sample_dtm,
  phi = nih_sample_topic_model$phi,
  theta = nih_sample_topic_model$theta
)

ll
