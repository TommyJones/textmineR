#' @name CalcLikelihood
#' @title Calculate the log likelihood of a DTM given a topic model
#' @param dtm = the document term matrix of class Matrix
#' @param phi = the phi matrix whose rows index topics and columns index words. The i, j entries are P(word_i | topic_j)  
#' @param theta = the theta matrix whose rows index documents and columns index topics. The i, j entries are P(topic_i | document_j)
#' @description
#'     This function takes a DTM, phi matrix (P(word|topic)), and a theta matrix (P(topic|document)) and
#'     returns a single value for the likelihood of the data given the model.
#' @examples
#'     myll <- CalcLikelihood(dtm=mydtm, phi=myphi, theta=mytheta)
#' @export
CalcLikelihood <- function(dtm, phi, theta, parallel=FALSE, cpus=NULL){
    # ensure that all inputs are sorted correctly
    phi <- phi[ colnames(theta) , colnames(dtm) ]
    
    theta <- theta[ rownames(dtm) , ]
    
    # If parallel = FALSE (I assume you like waiting...)
    ###############################################################
    if(! parallel){
        result <- CalcLikelihoodC(dtm=dtm, phi=phi, theta=theta)
        
        return(result) # function exits here if parallel=FALSE
    }
    
    
    # If parallel = TRUE ......
    ###############################################################
    
    if(is.null(cpus)){ # if you didn't specify the number of cpus...
        stop("You must specify the number of cpus when parallel=TRUE")
    }
    

    
    data_divided <- PartitionTopticModelData(dtm=dtm, theta=theta, partitions=cpus)
    
    sfInit(parallel=TRUE, cpus=cpus)
    sfLibrary(Matrix)
    sfLibrary(idaTopicModels)
    #     sfExport("CalcSumSquares")
    sfExport(list=c("phi"))
    
    result <- sfLapply(data_divided, function(x){
        tmp <- CalcLikelihoodC(dtm=x$dtm_divided, phi=phi, theta=x$theta_divided)
        
        tmp
    })
    
    sfStop()
    
    result <- sum(unlist(result))
    
    result
    
}
