\dontrun{
# Load pre-formatted data for use
data(nih_sample_dtm)
data(nih_sample)

result <- cluster_2_topic_model(
  dtm = nih_sample_dtm,
  clustering = nih_sample$IC_NAME
)
}
