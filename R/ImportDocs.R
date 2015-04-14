#' @title Import Documents for an in-development Shiny app.
#' @description This is the main document importing function. It is formatted
#' to work in the confines of an in-development Shiny app.
#' Those making an R script should *not*
#' use this function. Instead, use other functions, such as 
#' Vec2Dtm() directly.
#'
#' @param user.input = a list with elements directing the 
#'   method for creating a DTM from files located in a directory
#'
#' @details output: a list with two objects
#'       dtm = the document term matrix
#'       tf.mat = a dataframe of corpus-wide statistics
#' @export


ImportDocs <- function(user.input){
    vec <- Files2Vec(directory = user.input$input.dir)
    
    if( user.input$stopwords$custom$remove ){
        # if you selected a file for custom stopwords, then import it
        custom.stopwords <- scan(file = user.input$stopwords$datapath , what = "character", sep="\n")
    }else{
        # otherwise set custom.stopwords to NULL
        custom.stopwords <- NULL
    }
        
    dtm <- Vec2Dtm(vec = vec, 
                   min.n.gram = user.input$n.gram.length$min , 
                   max.n.gram = user.input$n.gram.length$min, 
                   remove.stopwords = user.input$stopwords$general$remove, 
                   custom.stopwords = user.input$stopwords$custom$words,
                   lower = user.input$lower, 
                   remove.punctuation = user.input$remove.punctuation, 
                   remove.numbers = user.input$remove.numbers,
                   stem.document=user.input$lemmatization$stem )
    
    if(user.input$lemmatization$depluralize){
        dtm <- DepluralizeDtm(dtm = dtm)
    }
    
    tf.mat <- TermDocFreq(dtm = dtm)
    
    return(list(dtm=dtm, tf.mat=tf.mat))
    
}
