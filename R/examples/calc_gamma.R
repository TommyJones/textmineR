# Load a pre-formatted dtm and topic model
data(nih_sample_topic_model)

# Make a gamma matrix, P(topic|words)
gamma <- calc_gamma(
  phi = nih_sample_topic_model$phi,
  theta = nih_sample_topic_model$theta
)
