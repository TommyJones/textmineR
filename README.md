# textmineR
Text mining functions, complimentary to J. Chang's lda package

This package implements various functions related to latent Dirichlet allocation (LDA). It is designed to work with the "lda" package by J. Chang.

Below is a demo of some of the functionality in `textmineR`


    library(textmineR)
    
    # Load some data into the workspace and convert it to a character vector
    data(crude)
    
    documents <- sapply(crude, function(x) x$content)
    
    
    # Create a document term matrix
    dtm <- Vec2Dtm(documents)
    
    dim(dtm)
    
    # explore basic frequencies & curate vocabulary
    tf <- TermDocFreq(dtm = dtm, min.ngram=1, max.ngram=2)
    
    vocabulary <- tf$term[ tf$term.freq > 1 & tf$doc.freq < nrow(dtm) / 2 ]
    
    dtm <- dtm[ , vocabulary ]
    
    # construct lexical objects to conform to the 'lda' package formats
    
    lex <- Dtm2Docs(dtm = dtm)
    
    lex <- lexicalize(lex, vocab=vocabulary, sep=" ")
    
    # fit an LDA model with some arbitrary parameters
    model <- lda::lda.collapsed.gibbs.sampler(documents = lex, 
                                              K = 10,
                                              alpha=0.1,
                                              eta=0.05,
                                              num.iterations=500,
                                              vocab = vocabulary)
    
    
    model <- FormatRawLdaOutput(lda.result = model, 
                                docnames = rownames(dtm),
                                smooth = T)
    
    names(model) # phi is P(words | topics), theta is P(topics | documents)
    
    # Calculate some summary statistics etc. Which is the real value-add of textmineR
    
    # phi-prime, P(topic | words) for classifying new documents
    model$phi_prime <- GetPhiPrime(phi = model$phi, theta = model$theta)
    
    # top 5 terms of the model according to phi & phi-prime
    model$top_terms <- GetTopTerms(phi = model$phi, M = 5)
    
    model$top_terms_prime <- GetTopTerms(phi = model$phi_prime, M = 5)
    
    # give a hard in/out assignment of topics in documents
    model$assignments <- model$theta
    
    model$assignments[ model$assignments < 0.05 ] <- 0
    
    model$assignments <- model$assignments / rowSums(model$assignments)
    
    model$assignments[ is.na(model$assignments) ] <- 0
    
    
    # Get some topic labels using n-grams from the DTM
    model$labels <- LabelTopics(assignments = model$assignments, 
                                dtm = dtm,
                                M = 2)
    
    # Probabilistic coherence: measures statistical support for a topic
    model$coherence <- apply(model$phi, 1, function(x){
      ProbCoherence(topic = x, dtm = dtm, M = 5)
    })
    
    
    # Number of documents in which each topic appears
    model$num_docs <- colSums(model$assignments > 0)
    
    # cluster topics together in a dendrogram
    
    model$topic_linguistic_dist <- HellDist(model$phi)
    
    model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
    
    model$hclust$clustering <- cutree(model$hclust, k = 4)
    
    model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1 ])
    
    plot(model$hclust)
    rect.hclust(model$hclust, k = length(unique(model$hclust$clustering)))
    
    # make a summary table
    model$summary <- data.frame(topic=rownames(model$phi),
                                cluster=model$hclust$clustering,
                                model$labels,
                                coherence=model$coherence,
                                num_docs=model$num_docs,
                                top_terms=apply(model$top_terms, 2, function(x){
                                  paste(x, collapse=", ")
                                }),
                                top_terms_prime=apply(model$top_terms_prime, 2, function(x){
                                  paste(x, collapse=", ")
                                }),
                                stringsAsFactors=FALSE)
    
    View(model$summary[ order(model$hclust$clustering) , ])

