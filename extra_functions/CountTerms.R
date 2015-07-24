#' @title Summarize a document term matrix subject to document frequency of words
#' @description This function is designed to work with an in-development shiny app. It's overkill to use on its own but you can if you want to.
#' @param freq.mat A data frame that is output from the TermDocFreq() function.
#' @param max.docs What is the maximum document frequency of words to include in this summary?
#' @param min.docs What is the minimum document frequency of words to include in this summary?
#' @export
#' @examples
#' term.freq <- TermDocFreq(dtm)
#' mycount <- CountTerms(freq.mat=term.freq, max.docs=nrow(dtm)/2, min.docs=5)
#' 
#' 

CountTerms <- function(freq.mat, max.docs, min.docs){
	keep.terms <- freq.mat$term[ freq.mat$doc.freq > min.docs & freq.mat$doc.freq < max.docs ]
	keep.index <- freq.mat$term %in% keep.terms
	result <- list(keep.terms=keep.terms, 
					summary.tf=summary(freq.mat$term.freq[ keep.index ]),
					summary.df=summary(freq.mat$doc.freq[ keep.index ]),
					summary.idf=summary(freq.mat$idf[ keep.index ]),
					num.terms=length(keep.terms), 
					num.uni=sum(! grepl("_", keep.terms)),
					num.bi=sum( grepl("^[a-z]+_[a-z]+$", keep.terms)),
					num.tri=sum( grepl("^[a-z]+_[a-z]+_[a-z]+$", keep.terms)),
					num.more=sum( grepl("^[a-z]+_[a-z]+_[a-z]+_[a-z]", keep.terms) )
					)
	return(result)
}
