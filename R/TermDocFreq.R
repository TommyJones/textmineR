#' @title Get term frequencies and document frequencies from a document term matrix.
#' @description This function takes a document term matrix as input and 
#' returns a data frame with columns for term frequency, document frequency, 
#' and inverse-document frequency
#' @param dtm A document term matrix of class \code{dgCMatrix}.
#' @return Returns a \code{data.frame} with 4 columns. The first column, 
#' \code{term} is a vector of token labels. The second column, \code{term.freq}
#' is the count of times \code{term} appears in the entire corpus. The third
#' column \code{doc.freq} is the count of the number of documents in which 
#' \code{term} appears. The fourth column, \code{idf} is the log-weighted
#' inverse document frequency of \code{term}.
#' @export
#' @examples
#' # Load a pre-formatted dtm and topic model
#' data(acq2) 
#' 
#' # Get the term frequencies 
#' term_freq_mat <- TermDocFreq(dtm)
#' 
#' str(term_freq_mat)

TermDocFreq <- function(dtm){
	freq.mat <- data.frame(term=colnames(dtm), 
	                       term.freq=Matrix::colSums(dtm), 
	                       doc.freq=Matrix::colSums(dtm > 0), 
	                       stringsAsFactors=FALSE)
	
	freq.mat$idf <- log(nrow(dtm) / freq.mat$doc.freq)
	return(freq.mat)
}
