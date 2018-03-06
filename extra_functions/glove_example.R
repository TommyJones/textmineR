
rm(list = ls())

library(textmineR)

data(nih_sample)

tcm <- CreateTcm(doc_vec = nih_sample$ABSTRACT_TEXT, skipgram_window = 5)

vocb <- GetVocabFromDtm(tcm)

glove <- text2vec::GlobalVectors$new(word_vectors_size = 10, 
                                     vocabulary = colnames(tcm), 
                                     x_max = 10)

glove$fit(tcm, n_iter = 20)
