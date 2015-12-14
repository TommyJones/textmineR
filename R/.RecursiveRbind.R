# recursively call rBind from the Matrix package. 
# This is used for combining a list into a large Matrix.
# This function is undocumented/internal to the package and not designed to be
# called by users.
.RecursiveRbind <- function(matrix_list){
  
  if(length(matrix_list) <= 100){
    
    return(do.call(Matrix::rBind, matrix_list))
    
  }else{
    batches <- seq(1, length(matrix_list), by = 100)
    
    matrix_list <- lapply(batches, function(x){
      do.call(Matrix::rBind, matrix_list[ x:min(x + 99, length(matrix_list))])
    })
    
    RecursiveRbind(matrix_list=matrix_list)
  }
  
}


