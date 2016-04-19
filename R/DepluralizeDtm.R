#' @title Run the CorrectS function on columns of a document term matrix.
#' @description Turns pluralizations of words in the columns of a document term 
#' matrix to their singular form. Then aggregates all columns that now have the 
#' same token. See example below. 
#' @param dtm A document term matrix of class \code{dgCMatrix} whose colnames are tokens
#' @param ... Other arguments to pass to \code{\link[textmineR]{TmParallelApply}}. See note, below.
#' @note
#' This function performs parallel computation by default. The default 
#' behavior is to use all available cores according to \code{\link[parallel]{detectCores}}.
#' However, this can be modified by passing the \code{cpus} argument when calling
#' this function.
#' @return
#' Returns a document term matrix of class \code{dgCMatrix}. The columns index
#' the de-pluralized tokens of the input document term matrix. In other words,
#' there will generally be fewer columns in the returned matrix than the
#' input matrix
#' 
#' @export
#' @examples
#' \dontrun{
#' myvec <- c("the quick brown fox eats chickens", 
#'            "the slow gray fox eats the slow chicken", 
#'            "look at my horse", "my horses are amazing")
#'            
#' names(myvec) <- paste("doc", 1:length(myvec), sep="_")
#' 
#' dtm <- Vec2Dtm(vec = myvec, min.n.gram = 1, max.n.gram = 1)
#' 
#' dtm_new <- DepluralizeDtm(dtm = dtm)
#' #' }

 
DepluralizeDtm <- function(dtm, ...){

	# run depluralization on column names of dtm
	adjust.terms <- CorrectS(colnames(dtm))
	
  # if we have adjustments to make
  if(sum(adjust.terms$changed) > 0){
    # begin procedure to merge depluralized columns with their singular form
    
    colnames(dtm) <- adjust.terms$adjusted
    
    # partition dtm.tmp on columns with duplicates (terms that had plurals) and columns without
    unchanged <- dtm[ , ! colnames(dtm) %in% colnames(dtm)[ duplicated(colnames(dtm)) ] ]
    changed <- dtm[ , colnames(dtm) %in% colnames(dtm)[ duplicated(colnames(dtm)) ] ]
    
    # get indices of columns to be merged
    term.indices <- TmParallelApply(X=unique(colnames(changed)),
                                    FUN=function(x){
                                      which(colnames(changed) == x)
                                    }, export=c("changed"), ...)
    
    gc()
    
    # merge columns by summation
    temp <- TmParallelApply(X = term.indices, 
                            FUN=function(y){
                              Matrix::Matrix(Matrix::rowSums(x = changed[ , y ]), 
                                     sparse=TRUE, ncol=1)
                            }, export=c("changed"), ...)
    gc()
    
    # put back together into a sparse matrix
    temp <- TmParallelApply(X = temp, FUN=function(x) Matrix::t(x), ...)
    
    temp <- RecursiveRbind(matrix_list = temp)
    
    temp <- Matrix::t(temp)
    
    colnames(temp) <- unique(colnames(changed))
    
    dtm <- Matrix::cBind(unchanged, temp)
  }
  
  dtm <- dtm[ , sort(colnames(dtm)) ]

	dtm

}
