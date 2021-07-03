# Load a pre-formatted dtm and topic model
data(nih_sample_dtm)
data(nih_sample_topic_model)

# Get the term frequencies
term_freq_mat <- term_doc_freq(nih_sample_dtm)

str(term_freq_mat)
