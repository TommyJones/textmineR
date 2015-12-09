#' An OS-independent parallel version of \code{lapply}
#' 
#' @description This function takes a vector or list and a function and applies in parallel.
#' @param X A vector or list over which to apply \code{FUN}
#' @param FUN A function to apply over \code{X}
#' @param cpus Number of CPU cores to use, defaults to the value returned by 
#' \code{parallel::detectCores()}.
#' @param export A character vector of objects in the workspace to export when 
#' using a Windows machine. Defauts to \code{NULL}
#' @details This function is used to parallelize executions in textmineR. It is 
#' necessary because of differing capabilities between Windows and Unix.
#' Unix systems use \code{mclapply} from \code{package:parallel}. Windows 
#' systems use \code{sfLapply} from \code{package:snowfall}.
#' @return This function returns a \code{list} of length \code{length(X)}.
#' @export
#' @examples
#' x <- 1:10000
#' f <- function(y) y * y + 12
#' result <- TmParallelApply(x, f)
TmParallelApply <- function(X, FUN, cpus=detectCores(), export=NULL){
  
  os <- .Platform$OS.type
  
  if(os == "unix" ){ # on unix systems use mclapply()
    
    out <- mclapply(X = X, FUN = FUN, mc.cores = cpus)
    
  }else{
    
    cl <- makeCluster(cpus)
    
    clusterEvalQ(cl, library(textmineR))
    
    if( ! is.null(export) ) clusterExport(varlist=export)
    
    out <- parLapply(x = X, fun = FUN)
    
    stopCluster(cl)
    
  }
  
  out
}

