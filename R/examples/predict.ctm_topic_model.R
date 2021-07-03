\dontrun{
# Load a pre-formatted dtm
data(nih_sample_dtm)

model <- FitCtmModel(
  dtm = nih_sample_dtm[1:20, ], k = 3,
  calc_coherence = FALSE, calc_r2 = FALSE
)

# Get predictions on the next 50 documents
pred <- predict(model, nih_sample_dtm[21:100, ])
}
