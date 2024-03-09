\dontrun{
data(nih_sample)

# DTM of unigrams and bigrams
dtm <- create_dtm(
  doc_vec = nih_sample$ABSTRACT_TEXT,
  doc_names = nih_sample$APPLICATION_ID,
  ngram_window = c(1, 2)
)

# DTM of unigrams with Porter's stemmer applied
dtm <- create_dtm(
  doc_vec = nih_sample$ABSTRACT_TEXT,
  doc_names = nih_sample$APPLICATION_ID,
  stem_lemma_function = function(x) SnowballC::wordStem(x, "porter")
)
}
