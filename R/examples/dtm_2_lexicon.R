\dontrun{
# Load pre-formatted data for use
data(nih_sample_dtm)

result <- Dtm2Lexicon(
  dtm = nih_sample_dtm,
  cpus = 2
)
}
