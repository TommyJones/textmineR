#' Function splits a DTM and theta by documents for parallelization
#' @description Function splits a DTM and theta by documents for parallelization.
#' This function is used by several other functions in \code{textmineR}. It 
#' shouldn't need to be used on its own unless you're customizing a solution.
#'
#' @param dtm = a documents X terms dimensional document term matrix in 
#'  sparse format from the Matrix package or a regular R matrix. 
#'   Will *not* work on DTMs from the tm package or simple triplet matrices from the slam package.
#' @param theta = a documents X topics dimensional matrix where each entry is p(topic|document)
#' @param partitions = number of threads over which you will parallelize.
#' 
#'       
#' @export
#' @examples
#' data_divided <- PartitionTopticModelData(dtm=mydtm, theta=mytheta, partitions=8)





PartitionTopticModelData <- function(dtm, theta, partitions=8){
    if(is.null(partitions)){ # if you didn't specify the number of partitions...
        stop("You must specify the number of partitions")
    }
    
    partitions <- round(partitions) # in case some bozo gives a non-integer value
    
    # divide the dtm by rows so it goes out to each processor
    breaks <- round(nrow(dtm) / partitions)
    
    indeces <- seq(from=1, to=nrow(dtm), by=breaks)
    
    data_divided <- lapply(indeces, function(j){
        dtm_divided <- dtm[ j:min(j + breaks - 1, nrow(dtm)) , ]
        theta_divided <- theta[ j:min(j + breaks - 1, nrow(dtm)) , ]
        
        list(dtm_divided=dtm_divided, theta_divided=theta_divided)
    })
    
    data_divided   
}





