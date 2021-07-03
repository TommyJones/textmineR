\dontrun{
data(nih_sample)

# TCM of unigrams and bigrams
tcm <- create_tcm(
  doc_vec = nih_sample$ABSTRACT_TEXT,
  skipgram_window = Inf,
  ngram_window = c(1, 2)
)

# TCM of unigrams and a skip=gram window of 3, applying Porter's word stemmer
tcm <- create_tcm(
  doc_vec = nih_sample$ABSTRACT_TEXT,
  skipgram_window = 3,
  stem_lemma_function = function(x) SnowballC::wordStem(x, "porter")
)
}
