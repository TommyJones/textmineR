#' An OS-independent parallel version of \code{lapply}
#' 
#' @description This function takes a vector or list and a function and applies in parallel.
#' @param X A vector or list over which to apply \code{FUN}
#' @param FUN A function to apply over \code{X}
#' @param cpus Number of CPU cores to use, defaults to the value returned by 
#' \code{parallel::detectCores()}.
#' @param export A character vector of objects in the workspace to export when 
#' using a Windows machine. Defauts to \code{NULL}
#' @param libraries A character vector of library/package names to load on to
#' each cluster if using a Windows machine. Defaults to \code{NULL}
#' @details This function is used to parallelize executions in textmineR. It is 
#' necessary because of differing capabilities between Windows and Unix.
#' Unix systems use \code{mclapply} from \code{package:parallel}. Windows 
#' systems use \code{parLapply} from \code{package:parallel}.
#' @return This function returns a \code{list} of length \code{length(X)}.
#' @export
#' @examples
#' \dontrun{
#' x <- 1:10000
#' f <- function(y) y * y + 12
#' result <- TmParallelApply(x, f)
#' }
TmParallelApply <- function(X, FUN, cpus=parallel::detectCores(), 
                            export=NULL, libraries=NULL){
  
  os <- .Platform$OS.type
  
  if(os == "unix" ){ # on unix systems use mclapply()
    
    out <- parallel::mclapply(X = X, FUN = FUN, mc.cores = cpus)
    
  }else{
    
    cl <- parallel::makeCluster(cpus)
    
    parallel::clusterEvalQ(cl, library(textmineR))
    
    # export & load libraries
    if(! is.null(libraries) ){
      
      lib_fun <- function(){
        for(l in libraries){
          eval(parse(text = paste("library(", l, ")", sep="")))
        }
      }
      parallel::clusterExport(cl, varlist = c("libraries", "lib_fun"))
      
      parallel::clusterCall(cl, fun=lib_fun)
      
    }
    
    # export any other objects needed
    if( ! is.null(export) ) parallel::clusterExport(cl, varlist=export)
    
    out <- parallel::parLapply(cl , X = X, fun = FUN)
    
    parallel::stopCluster(cl)
    
  }
  
  out
}

