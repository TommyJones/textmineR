#' Convert a DTM to a Character Vector of documents
#' 
#' @description This function takes a sparse matrix (DTM) as input and returns a character vector
#' whose length is equal to the number of rows of the input DTM.
#' @param dtm A sparse Matrix from the matrix package whose rownames correspond to documents and colnames correspond to words
#' @param parallel Do you want to parallelize this function using snowfall? Default is FALSE 
#' @param cpus If parallel is TRUE, the number of threads to use. (Recommendation is 4, for memory's sake)
#' @export
#' @examples
#' Dtm2Docs(dtm=mydtm, parallel=TRUE, cpus=4)

Dtm2Docs <- function(dtm, parallel=FALSE, cpus=NULL){
    
    # define a function to perform on a dtm row
    MakeDoc <- function(dtm_row){
        
        tmp <- dtm_row[ dtm_row > 0 ]
        
        # if there are no words in the dtm, then return an empty character
        if( length(tmp) == 0 ){
            return(" ")
        }
        
        # Assuming all systems go, create the document
        vocab <- names(tmp)
        
        doc <- sapply(1:length(vocab), function(k){
            rep(x = vocab[ k ], times = tmp[ k ])
        })
        
        doc <- paste(unlist(doc), collapse=" ")
        
        return(doc)
    }
    
    
    #### If don't want to parallelize (and you like waiting...)
    if( ! parallel ){
        result <- sapply(1:nrow(dtm), function(j){
            MakeDoc(dtm_row = dtm[ j , ])
        })
        
        names(result) <- rownames(dtm)
        
        return(result)  # function exits here if parallel=FALSE
        
        ### If you do want to parallelize
    }else{
        if(is.null(cpus)){
            stop("You must specify the number of cpus when parallel=TRUE")
        }
        
        cpus <- round(cpus) # in case some bozo gives a non-integer value
        
        # divide the dtm by rows so it goes out to each processor
        breaks <- round(nrow(dtm) / cpus)
        
        indeces <- seq(from=1, to=nrow(dtm), by=breaks)
        
        dtm_divided <- lapply(indeces, function(j){
            dtm[ j:min(j + breaks - 1, nrow(dtm)) , ]
        })
        
        sfInit(parallel=TRUE, cpus=cpus)
        sfLibrary(Matrix)
        sfExport(list=c("MakeDoc"))
        
        result <- sfLapply(dtm_divided, function(ROWS){
            sapply(1:nrow(ROWS), function(j){
                MakeDoc(dtm_row = ROWS[ j , ])
            })
        })
        
        sfStop()
        
        result <- unlist(result)
        
        names(result) <- rownames(dtm)
        
        return(result) # function exits here if parallel=TRUE
    }    
}

