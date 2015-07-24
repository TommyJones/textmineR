#' @title Run the CorrectS function on columns of a document term matrix of class Matrix.
#' @description Turns pluralizations of words in the columns of a document term matrix to their singular form. Then aggregates
#' all columns that now have the same token. See example below. This function **requires** snowfall.
#' @param dtm A document term matrix of class Matrix whose colnames are words or n-grams
#' 
#' 
#' @export
#' @examples
#' myvec <- c("the quick brown fox eats chickens", "the slow gray fox eats the slow chicken", "look at my horse", "my horses are amazing")
#' names(myvec) <- paste("doc", 1:length(myvec), sep="_")
#' 
#' dtm <- Vec2Dtm(vec = myvec, min.n.gram = 1, max.n.gram = 1)
#' 
#' colnames(dtm)
#' [1] "amazing"  "brown"    "chicken"  "chickens" "eats"     "fox"      "gray"     "horse"    "horses"   "quick"    "slow"   
#' 
#' dtm
#' 4 x 11 sparse Matrix of class "dgCMatrix"
#'    [[ suppressing 11 column names ‘amazing’, ‘brown’, ‘chicken’ ... ]]
#'                            
#' doc_1 . 1 . 1 1 1 . . . 1 .
#' doc_2 . . 1 . 1 1 1 . . . 2
#' doc_3 . . . . . . . 1 . . .
#' doc_4 1 . . . . . . . 1 . .
#'
#' dtm_new <- DepluralizeDtm(dtm = dtm)
#' 
#' dtm_new
#' 4 x 9 sparse Matrix of class "dgCMatrix"
#'       amazing brown eat fox gray quick slow chicken horse
#' doc_1       .     1   1   1    .     1    .       1     .
#' doc_2       .     .   1   1    1     .    2       1     .
#' doc_3       .     .   .   .    .     .    .       .     1
#' doc_4       1     .   .   .    .     .    .       .     1




DepluralizeDtm <- function(dtm){

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
                                    }, export=c("changed"))
    
    gc()
    
    # merge columns by summation
    temp <- TmParallelApply(X = term.indices, 
                            FUN=function(y){
                              Matrix(Matrix::rowSums(x = changed[ , y ]), 
                                     sparse=TRUE, ncol=1)
                            }, export=c("changed"))
    gc()
    
    # put back together into a sparse matrix
    temp <- TmParallelApply(X = temp, FUN=function(x) Matrix::t(x))
    
    temp <- RecursiveRbind(matrix_list = temp)
    
    temp <- Matrix::t(temp)
    
    colnames(temp) <- unique(colnames(changed))
    
    dtm <- cBind(unchanged, temp)
  }
  
  dtm <- dtm[ , sort(colnames(dtm)) ]

	dtm

}
