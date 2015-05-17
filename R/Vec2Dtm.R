#' @title Convert a character vector to a document term matrix of class Matrix.
#' @description This is the main document term matrix creating function for \code{textmineR}.
#' In most cases, all you need to do is import documents as a character vector in R and then run this function
#' to get a document term matrix that is compatible with the rest of \code{textmineR}'s functionality.
#' 
#' @param vec A character vector of documents. Punctuation etc. is allowed. names(vec) should be names of your documents.
#' @param min.n.gram The minimum size of n for creating n-grams. (Defaults to 1)
#' @param max.n.gram The maximum size of n for creating n-grams. (Defaults to 2. Numbers greater than 3 are discouraged due to risk of overfitting.)
#' @param remove.stopwords Do you want to remove standard stopwords from your documents? (Defaults to TRUE)
#' @param custom.stopwords If not NULL (the default) a character vector of additional stopwords to remove from your corpus. Note: it is generally faster to
#' remove additional stopwords by subsetting the columns of a DTM post-hoc. This is to be used when you want these words removed before creatign the DTM.
#' @param lower Do you want all words coerced to lower case? (Defaults to TRUE)
#' @param remove.punctuation Do you want to convert all punctuation to spaces? For example, "big-lipped fish" goes to "big lipped fish" (Defaults to TRUE)
#' @param remove.numbers Do you want to convert all numbers to spaces? For example, "3rd grade teachers" goes to " rd grade teachers" (Defaults to TRUE)
#' @param stem.document Do you want to stem the words in your document? (Defaults to FALSE)
#'
#' @export
#' @examples
#' myvec <- c("the quick brown fox", "the slow gray fox", "look at my horse", "my horse is amazing")
#' names(myvec) <- paste("doc", 1:length(myvec), sep="_")
#' 
#' dtm <- Vec2Dtm(vec = myvec, min.n.gram = 1, max.n.gram = 1)
#' 
#' dtm
#' 
#' 4 x 7 sparse Matrix of class "dgCMatrix"
      #' amazing brown fox gray horse quick slow
#' doc_1       .     1   1    .     .     1    .
#' doc_2       .     .   1    1     .     .    1
#' doc_3       .     .   .    .     1     .    .
#' doc_4       1     .   .    .     1     .    .
#' 

Vec2Dtm <- function(vec, min.n.gram=1, max.n.gram=2, remove.stopwords=TRUE, custom.stopwords=NULL, lower=TRUE, remove.punctuation=TRUE, remove.numbers=TRUE, stem.document=FALSE){
	# for now, it is strongly advised to accept the defaults for lower, remove.punctuation, and remove.numbers
	# Other functions are built assuming that the column headings of a dtm contain only letters and underscores "_"
	
  # set an option that normally causes this to crash on a Mac
  if(grepl("apple", sessionInfo()$platform)) options(mc.cores=1)
  
	if(remove.stopwords){
		stopwords <- unique(c(stopwords("english"), stopwords("SMART")))
	}else{
		stopwords <- c()
	}
  
  docnames <- names(vec)
    
    if( ! is.null(custom.stopwords) ) stopwords <- c(stopwords, custom.stopwords)
	
	if( lower ) vec <- tolower(vec)
	
	if( remove.punctuation ){ 
		vec <- gsub("[^a-zA-Z0-9]", " ", vec)
		stopwords <- gsub("[^a-zA-Z0-9]", " ", stopwords)
		stopwords <- unique(unlist(strsplit(stopwords, split="\\s+")))
	}
	
	if( remove.numbers ){ 
		vec <- gsub("[0-9]", " ", vec)
	}

	vec <- gsub("\\s+", " ", vec) # remove extra spaces
	
	corp <- Corpus(VectorSource(vec))
    
    
    if( remove.stopwords | ! is.null(custom.stopwords)){
        corp <- tm_map(x=corp, removeWords, stopwords)
    }
	
	if( stem.document ){
		corp <- tm_map(x=corp, stemDocument)
	}
	
	if(max.n.gram == 1){
		dtm <- DocumentTermMatrix(corp)
	}else{
		dtm <- DocumentTermMatrix(corp, control=list(tokenize=NgramTokenizer(min=min.n.gram, max=max.n.gram)))
	}
	
	dtm <- MakeSparseDTM(dtm=dtm)
	
	colnames(dtm) <- gsub(" ", "_", colnames(dtm))
  
  rownames(dtm) <- docnames
	
	return(dtm)
}
