# Load a pre-formatted dtm and topic model
data(nih_sample_topic_model)
data(nih_sample_dtm)

calc_prob_coherence(phi = nih_sample_topic_model$phi,
                    dtm = nih_sample_dtm, M = 5)
