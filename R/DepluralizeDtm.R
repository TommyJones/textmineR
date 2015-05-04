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
	
	# begin procedure to merge depluralized columns with their singular form

    colnames(dtm) <- adjust.terms$adjusted

	# partition dtm.tmp on columns with duplicates (terms that had plurals) and columns without
	unchanged <- dtm[ , ! colnames(dtm) %in% colnames(dtm)[ duplicated(colnames(dtm)) ] ]
	changed <- dtm[ , colnames(dtm) %in% colnames(dtm)[ duplicated(colnames(dtm)) ] ]

	# get indices of columns to be merged
	sfInit(parallel=TRUE, cpus=4)
	sfExport("changed")
    sfLibrary(Matrix)
    
	term.indices <- sfLapply( unique(colnames(changed)), function(x){ 	    
        which(colnames(changed) == x)
    })
	
    sfStop()
    
    gc()
    
	# merge columns by summation
    
#     merge.list <- lapply(term.indices, function(y) changed[ , y ])
    
    
	sfInit(parallel=TRUE, cpus=4)
	sfExport("changed")
	sfLibrary(Matrix)
	
	temp <- sfLapply( term.indices, function(y){
		result <- Matrix(Matrix::rowSums(x = changed[ , y ]), sparse=TRUE)
	})
    
    sfStop()

    gc()

	# put back together into a sparse matrix
	batches <- seq(1, length(temp), by=100)
    
    if(length(batches) > 1){ # if we need snowfall at all...
        sfInit(parallel=TRUE, cpus=4)
        sfExport("temp")
        sfLibrary(Matrix)

        temp2 <- sfLapply(batches, function(x){
            do.call(cBind, temp[ x:min( x + 99, length(temp) ) ])
        })
        
        sfStop()
    }else{
        temp2 <- temp
    }
    
    if(length(temp2) > 100 ){ # if temp2 is still really big
        
        batches <- seq(1, length(temp2), by=100)
        
        sfInit(parallel=TRUE, cpus=4)
        sfExport("temp2")
        sfLibrary(Matrix)
        
        temp <- sfLapply(batches, function(x){
            do.call(cBind, temp2[ x:min( x + 99, length(temp2) ) ])
        })
        
        sfStop()
        
        gc()
        
        temp <- do.call(cBind, temp)
        
    }else{
        temp <- do.call(cBind, temp2)
    }

    colnames(temp) <- unique(colnames(changed))

	dtm <- cBind(unchanged, temp)

	return(dtm)

#     return(list(changed=changed, term.indices=term.indices))
}
