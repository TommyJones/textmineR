FitStmModel <- function(dtm, k, ...){
  message("FitStmModel is under active development. Additional args *must* be ",
          "passed to stm::stm through ... \nDefault parameters and prompts will be", 
          " added as FitStmModel matures.")
  
  corp <- stm::readCorpus(dtm, "Matrix")
  
  fit <- stm::stm(documents = corp$documents,
                  vocab = colnames(dtm),
                  K = k,
                  ...)
  
  result <- FormatRawStmOutput(stm_result = fit,
                               docnames = rownames(dtm), 
                               vocab = colnames(dtm))
  
  result
}