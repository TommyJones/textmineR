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
#' 
#'       
#' @export
#' @examples
#' r2 <- CalcTopicModelR2(dtm=mydtm, phi=lda$phi, theta=lda$theta, parallel=TRUE, cpus=8)


CalcTopicModelR2 <- function(dtm, phi, theta){
    
    # ensure that all inputs are sorted correctly
    phi <- phi[ colnames(theta) , colnames(dtm) ]
    
    theta <- theta[ rownames(dtm) , ]
    
    
    # get ybar, the "center" of the documents
    ybar <- Matrix::colMeans(dtm)
    
    # do in parallel in batches of about 3000 if we have more than 3000 docs
    if(nrow(dtm) > 3000){
      
      batches <- seq(1, nrow(dtm), by = 3000)
      
      data_divided <- lapply(batches, function(j){
        
        dtm_divided <- dtm[ j:min(j + 2999, nrow(dtm)) , ]
        
        theta_divided <- theta[ j:min(j + 2999, nrow(dtm)) , ]
        
        list(dtm_divided=dtm_divided, theta_divided=theta_divided)
      })
      
      result <-TmParallelApply(X = data_divided, FUN = function(x){
        CalcSumSquares(dtm = x$dtm_divided, 
                       phi = phi, 
                       theta = x$theta_divided, 
                       ybar=ybar)
      }, export=c("phi", "ybar"))
      
      result <- do.call(rbind, result)
      
      result <- 1 - sum(result[ , 1 ]) / sum(result[ , 2 ])
    
    # do sequentially otherwise
    }else{
      sum_squares <- CalcSumSquares(dtm = dtm,  phi = phi, theta = theta, ybar=ybar)
      
      result <- 1 - sum_squares[ 1 ] / sum_squares[ 2 ]
      
    }
    
    result
}