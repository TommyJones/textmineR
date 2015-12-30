# textmineR
Functions for Text Mining and Topic Modelin

An aid for text mining in R, with a syntax that
    is more familiar to experienced R users. Also, implements various functions
    related to topic modeling. It works well with with the "lda" package by J.
    Chang.
    
Below is a demo of some of the functionality in `textmineR`


    library(textmineR)

    # Load some data into the workspace and convert it to a character vector
    data(acq2)
    
    # Create a document term matrix
    dtm <- Vec2Dtm(documents, min.n.gram=1, max.n.gram=2)
    
    dim(dtm)
    
    # explore basic frequencies & curate vocabulary
    tf <- TermDocFreq(dtm = dtm)
    
    vocabulary <- tf$term[ tf$term.freq > 1 & tf$doc.freq < nrow(dtm) / 2 ]
    
    dtm <- dtm[ , vocabulary ]
    
    dim(dtm)
    
    # fit some LDA models and select the best number of topics
    k_list <- seq(5, 50, by=5)
    
    
    model_list <- TmParallelApply(X = k_list, FUN = function(k){
      m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
      m$coherence <- apply(m$phi, 1, function(x) ProbCoherence(topic = x, dtm = dtm, M = 5))
      m
    }, export=c("dtm")) # export only needed for Windows machines
    
    coherence_mat <- data.frame(k=sapply(model_list, function(x) nrow(x$phi)), 
                                coherence=sapply(model_list, function(x) mean(x$coherence)), stringsAsFactors=F)
    
    plot(coherence_mat, type="o")
    
    # select k based on maximum average coherence
    model <- model_list[ coherence_mat$coherence == max(coherence_mat$coherence) ][[ 1 ]]

    
    names(model) # phi is P(words | topics), theta is P(topics | documents)
    
    # Calculate some summary statistics etc. Which is the real value-add of textmineR
    
    # Get the R-squared of this model
    model$r2 <- CalcTopicModelR2(dtm = dtm, phi = model$phi, theta=model$theta)
    
    model$r2
    
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
    
    model$hclust$clustering <- cutree(model$hclust, k = 10)

    
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
    