#' Word Cloud for Topics
#' @description Takes A numeric vector whose names are the terms we wish to 
#' display. Does not return a value; plots the word cloud.
#' @param term.freq.vec A numeric vector whose entries represent word frequency 
#' and names represent words
#' @param title A character for the title you want for your word cloud. 
#' @param outfilepath A character for the path to save your word cloud. Do not 
#' include file name; that is created from title.
#' @export
#' @return Does not return an R object. Instead, this function saves a word 
#' cloud to the location given by \code{paste(outfilepath, title, ".png", sep="")}
#' @examples
#' \dontrun{
#' for( j in 1:nrow(phi)){
#'   TopicWordCloud(term.freq.vec=phi[ j  , ], 
#'                  title=rownames(phi)[ j ], 
#'                  outfilepath="mypath/wordclouds/")
#' }
#' }


TopicWordCloud <- function(term.freq.vec, title="", outfilepath=""){
    
    df <- data.frame(term=names(term.freq.vec)[ term.freq.vec > 0],  
                     freq=term.freq.vec[ term.freq.vec > 0])
    
    df$freq <- round(df$freq/max(df$freq) * 100)
    
    col <- c("#313695", rev(RColorBrewer::brewer.pal(4, "RdYlBu")))
    
    png( paste(outfilepath, title, ".png", sep=""), width=8, 
         height=8, units='in', res=300)
    
        par(bg="#F8F8FF")
        
        wordcloud::wordcloud(words=df$term, freq=df$freq, scale=c(4, 0.5), 
                             min.freq=1, max.words=100, 
                             colors=col, random.order=FALSE)
        
        title(main=title, line=-2)
    dev.off()
}
