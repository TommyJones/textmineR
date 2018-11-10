# Corpus is just a character vector

docs <- c("This is my first document.",
          "My 2nd document!",
          "skills, son, skills. Skillz!",
          "all my documents have skills")

# Single function call to make a DTM
d <- CreateDtm(doc_vec = docs[2:4], doc_names = seq_along(docs)[2:4],
               ngram_window = c(1,2),
               stopword_vec = "the", 
               lower = TRUE,
               remove_punctuation = TRUE,
               remove_numbers = TRUE,
               cpus = 2)

# Fit a model
m <- FitLsaModel(dtm = d, k = 2)

# Make a DTM for a new document
d2 <- CreateDtm(doc_vec = docs[1], doc_names = 1,
                ngram_window = c(1,2),
                stopword_vec = "the", 
                lower = TRUE,
                remove_punctuation = TRUE,
                remove_numbers = TRUE,
                cpus = 2)

# Single call to predict
p <- predict(m, d2)
