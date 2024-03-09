# Load a pre-formatted dtm and topic model
data(nih_sample_topic_model)

top_terms <- get_top_terms(phi = nih_sample_topic_model$phi, M = 5)

str(top_terms)
