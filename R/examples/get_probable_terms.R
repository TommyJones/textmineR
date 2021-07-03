# Load a pre-formatted dtm and topic model
data(nih_sample_topic_model)
data(nih_sample_dtm)

# documents with a topic proportion of .25 or higher for topic 2
mydocs <- rownames(
    nih_sample_topic_model$theta
)[nih_sample_topic_model$theta[, 2] >= 0.25]

term_probs <- Matrix::colSums(nih_sample_dtm) /
    sum(Matrix::colSums(nih_sample_dtm))

get_probable_terms(docnames = mydocs,
                   dtm = nih_sample_dtm,
                   p_terms = term_probs)
