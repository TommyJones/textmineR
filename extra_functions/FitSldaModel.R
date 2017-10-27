


FitSldaModel <- function(dtm, k, e_iterations, m_iterations,
                         annotations, params, variance,
                         alpha = 0.1, beta = 0.05, smooth = TRUE, 
                         return_all = FALSE, ...){
  
  # Some setup to match arguments given with ...
  dots <- list(...)
  
  ### Check consistency of inputs ----
  # TODO add checks
  
  ### Format documents for entry into lda::lexicalize ----
  vocab <- colnames(dtm)
  
  lex_args <- c(list(dtm = dtm), dots[ names(dots) %in% 
                                         names(formals(textmineR::TmParallelApply)) ])
  
  lex <- do.call(textmineR::Dtm2Docs, lex_args)
  
  if(nrow(dtm) > 1000){
    chunks <- seq(1, nrow(dtm), by = 1000)
    
    lex_list <- lapply(chunks, function(x) lex[ x:min(x + 999, length(lex)) ])
    
    lex_args <- c(list(X = lex_list, 
                       FUN = function(x){
                         lda::lexicalize(x, sep = " ", vocab = vocab)
                       },
                       export = "vocab"),
                  dots[ names(dots) %in% 
                          names(formals(textmineR::TmParallelApply)) ])
    
    lex <- do.call(textmineR::TmParallelApply, lex_args)
    
    lex <- do.call(c, lex)
    
  }else{
    lex <- lda::lexicalize(lex, sep=" ", vocab=vocab)
  }
  
  
  ### Prepare to fit a model ----
  model_args <- c(list(documents = lex, 
                       K = k, 
                       vocab = vocab, 
                       num.e.iterations = e_iterations, 
                       num.m.iterations = m_iterations,
                       alpha = alpha, 
                       eta = beta,
                       annotations = annotations, 
                       params = params, 
                       variance = variance),
                  dots[ names(dots) %in% 
                          names(formals(lda::slda.em)) ] )
  
  model <- do.call(lda::slda.em, model_args)
  
  result <- textmineR::FormatRawLdaOutput(lda_result = model, 
                                          docnames = rownames(dtm), 
                                          smooth = smooth)
  
  result <- c(result, etc = model)
  
  return(result)
  
}