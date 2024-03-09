# Load a pre-formatted dtm and topic model
data(nih_sample)
data(nih_sample_dtm)

# see the original documents
nih_sample$ABSTRACT_TEXT[1:3]

# see the new documents re-structured from the DTM
new_docs <- dtm_2_docs(dtm = nih_sample_dtm)

new_docs[1:3]
