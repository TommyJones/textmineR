#' Calculate a distance function using parallelization from the snowfall package
#' @description This function calculates distance between rows or columns of matrix "x"
#' 
#' @param x = an object of class matrix or data.frame
#' @param dist.function = a function for claculating distance between the vectors of 
#'       x. Note, dist.function must take two numeric vectors as input and return a 
#'       numeric output. Functions that do not meet these critera may not work.
#' @param by.rows =  a logical. Do you want to get distrance of the rows in your x matrix? defaulting to TRUE
#' @export


CalcParallelDist <- function(x, dist.function, by.rows=TRUE, cpus=8){    
    if(! by.rows ) x <- t(x)
    
    rows <- nrow(x)
    
    sfInit(parallel=T, cpus=cpus)
    sfExport(list=c("dist.function", "x", "rows"))
    
    result <- sfLapply(1:(rows - 1), function(j){
        drow <- rep(0, rows)
        
        for(k in (j + 1):rows ){
            drow[ k ] <- dist.function(x[ j , ], x[ k , ])
        }
        
        return(drow)
    })
    
    sfStop()
    
    result <- rbind(do.call(rbind, result), rep(0, rows))
    
    rownames(result) <- rownames(x)
    colnames(result) <- rownames(x)
    
    result <- result + t(result)
    
    return(result)
}
