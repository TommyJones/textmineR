#' Function to calculate R-squared of a topic model.
#' @description Function to calculate R-squared for a topic model using pure R code. 
#' Note: This function is slower than CalcTopicModelR2, which is a C++ function. 
#' However, TopicModelR2 gives you more output, such as the sse, ssm, and sst for each document.
#' This uses a geometric interpretation of R-squared as the proportion of total distance 
#' each document is from the center of all the documents that is explained by the model. 
#' Note: all input matrices must have rownames and colnames
#'
#' @param dtm = a documents X terms dimensional document term matrix in 
#'  sparse format from the Matrix package or a regular R matrix. 
#'   Will *not* work on DTMs from the tm package or simple triplet matrices from the slam package.
#' @param phi = a topics X terms dimensional matrix where each entry is p(term|topic)
#' @param theta = a documents X topics dimensional matrix where each entry is p(topic|document)
#' @param parallel = a logical. Do you have snowfall installed? Would you like to parallelize?
#' @param cpus = number of threads over which to parallelize.
#' 
#' @details
#' Output:
#' a list with 3 elements - 
#'   r2 = R-squared of the model
#'   sse = the sum of squared errors. This is a vector, the square root of which
#'       gives the l2-norm or euclidean distance from each document to its fitted value
#'   sst = the total sum of squares. This is a vector, the square root of which 
#'       gives the l2-norm or euclidean distance from each document to the "mean" document.
#'   ssm = the model sum of squares. This is a vector, the square root of which 
#'       gives the l2-norm or euclidean distance from the fitted-value for each document to the "mean" document.
#'       
#' @export
#' @examples
#' r2 <- TopicModelR2(dtm=mydtm, phi=lda$phi, theta=lda$theta, parallel=TRUE, cpus=8)


TopicModelR2 <- function(dtm, phi, theta, parallel=FALSE, cpus=NULL){
    
    # ensure that all inputs are sorted correctly
    phi <- phi[ colnames(theta) , colnames(dtm) ]
    
    theta <- theta[ rownames(dtm) , ]
    
    # get ybar, the "average" document
    ybar <- Matrix::colMeans(dtm)
    
    
    # Define a function to act upon one document
    
    SSFun <- function(y, theta.row, phi, ybar){
        yhat <- sum(y) * theta.row %*% phi
        
        diff1 <- y - yhat
        diff2 <- y - ybar
        diff3 <- yhat - ybar
        
        sse <- sum(diff1 * diff1)
        sst <- sum(diff2 * diff2)
        ssm <- sum(diff3 * diff3)
        
        data.frame(sse=sse, sst=sst, ssm=ssm)
    }
    
    # If parallel = FALSE (I assume you like waiting...)
    ###############################################################
    if(! parallel){
        result <- lapply(1:nrow(dtm), function(j){
            SSFun(y=dtm[ j , ], theta.row=theta[ j , ], phi=phi, ybar=ybar)
        })
        
        result <- do.call(rbind, result)
        
        result <- list(r2=NA, sse=result$sse, sst=result$sst, ssm=result$ssm)
        
        result$r2 <- 1 - sum(result$sse) / sum(result$sst)
        
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
    sfExport(list=c("ybar", "phi", "SSFun"))
    
    result <- sfLapply(data_divided, function(x){
        tmp <- lapply(1:nrow(x$dtm_divided), function(j){
            SSFun(y=x$dtm_divided[ j , ], theta.row=x$theta_divided[ j , ], phi=phi, ybar=ybar)
        })
        tmp <- do.call(rbind, tmp)
        
        tmp
    })
    
    sfStop()
    
    result <- do.call(rbind, result)
    
    result <- list(r2=NA, sse=result$sse, sst=result$sst, ssm=result$ssm)
    
    result$r2 <- 1 - sum(result$sse) / sum(result$sst)
    
    return(result) # function exits here if parallel=TRUE
}