#' Function to calculate R-squared of a topic model.
#' @description Function to calculate R-squared for a topic model. 
#' This uses a geometric interpretation of R-squared as the proportion of total distance 
#' each document is from the center of all the documents that is explained by the model. 
#'
#' @param dtm = a documents X terms dimensional document term matrix in 
#'  sparse format from the Matrix package or a regular R matrix. 
#'   Will *not* work on DTMs from the tm package or simple triplet matrices from the slam package.
#' @param phi = a topics X terms dimensional matrix where each entry is p(term|topic)
#' @param theta = a documents X topics dimensional matrix where each entry is p(topic|document)
#' @param parallel = a logical. Do you have snowfall installed? Would you like to parallelize?
#' @param cpus = number of threads over which to parallelize.
#' 
#'       
#' @export
#' @examples
#' r2 <- CalcTopicModelR2(dtm=mydtm, phi=lda$phi, theta=lda$theta, parallel=TRUE, cpus=8)


CalcTopicModelR2 <- function(dtm, phi, theta, parallel=FALSE, cpus=NULL){
    
    # ensure that all inputs are sorted correctly
    phi <- phi[ colnames(theta) , colnames(dtm) ]
    
    theta <- theta[ rownames(dtm) , ]
    
    
    # get ybar, the "center" of the documents
    ybar <- Matrix::colMeans(dtm)
    
    # If parallel = FALSE (I assume you like waiting...)
    ###############################################################
    if(! parallel){
        sum_squares <- CalcSumSquares(dtm = dtm, phi = phi, theta = theta, ybar=ybar)
        
        result <- 1 - sum_squares[ 1 ] / sum_squares[ 2 ]
        
        return(result) # function exits here if parallel=FALSE
    }
    
    
    # If parallel = TRUE ......
    ###############################################################
    
    if(is.null(cpus)){ # if you didn't specify the number of cpus...
        stop("You must specify the number of cpus when parallel=TRUE")
    }
    
    cpus <- round(cpus) # in case some bozo gives a non-integer value
    
    # divide the dtm by rows so it goes out to each processor
    breaks <- round(nrow(dtm) / cpus)
    
    indeces <- seq(from=1, to=nrow(dtm), by=breaks)
    
    data_divided <- lapply(indeces, function(j){
        dtm_divided <- dtm[ j:min(j + breaks - 1, nrow(dtm)) , ]
        theta_divided <- theta[ j:min(j + breaks - 1, nrow(dtm)) , ]
        
        list(dtm_divided=dtm_divided, theta_divided=theta_divided)
    })
    
    sfInit(parallel=TRUE, cpus=cpus)
    sfLibrary(Matrix)
    sfLibrary(textmineR)
#     sfExport("CalcSumSquares")
    sfExport(list=c("phi", "ybar"))
    
    result <- sfLapply(data_divided, function(x){
        tmp <- CalcSumSquares(dtm = x$dtm_divided, phi = phi, theta = x$theta_divided, ybar=ybar)
        
        tmp
    })
    
    sfStop()
    
    result <- do.call(rbind, result)
    
    result <- 1 - sum(result[ , 1 ]) / sum(result[ , 2 ])
        
    return(result) # function exits here if parallel=TRUE
}