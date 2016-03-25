#' @title Convert a character vector to a document term matrix of class Matrix.
#' @description This is the main document term matrix creating function for \code{textmineR}.
#' In most cases, all you need to do is import documents as a character vector in R and then run this function
#' to get a document term matrix that is compatible with the rest of \code{textmineR}'s functionality.
#' 
#' @param vec A character vector of documents. Punctuation etc. is allowed. names(vec) should be names of your documents.
#' @param docnames A vector of names for your documents. Defaults to 
#'        \code{names(doc_vec)}. If NULL, then docnames is set to be 
#'        \code{1:length(doc_vec)}.
#' @param min.n.gram The minimum size of n for creating n-grams. (Defaults to 1)
#' @param max.n.gram The maximum size of n for creating n-grams. (Defaults to 1. Numbers greater than 3 are discouraged due to risk of overfitting.)
#' @param remove.stopwords Do you want to remove standard stopwords from your documents? (Defaults to TRUE)
#' @param custom.stopwords If not NULL (the default) a character vector of additional stopwords to remove from your corpus. Note: it is generally faster to
#' remove additional stopwords by subsetting the columns of a DTM post-hoc. This is to be used when you want these words removed before creatign the DTM.
#' @param lower Do you want all words coerced to lower case? (Defaults to TRUE)
#' @param remove.punctuation Do you want to convert all punctuation to spaces? For example, "big-lipped fish" goes to "big lipped fish" (Defaults to TRUE)
#' @param remove.numbers Do you want to convert all numbers to spaces? For example, "3rd grade teachers" goes to " rd grade teachers" (Defaults to TRUE)
#' @param stem.document Do you want to stem the words in your document? (Defaults to FALSE)
#' @param ... Additional arguments to be passed to \code{textmineR::TmParallelApply}
#' @note
#' This function relies heavily on the \code{tm} and \code{RWeka} packages. N-grams are derived using the \code{RWeka} package. 
#' There is a confilct between \code{RWeka} and \code{parallel} (used for multithread processing on unix-like systems).
#' Consequently, using n-grams (n > 1) causes construction of the DTM to be considerably slower on unix systems. 
#' Speed is unaffected on Windows machines because parallelization is not supported for DTM construction.
#'
#' @export
#' @examples
#' \dontrun{
#' data(nih_sample)
#' 
#' dtm <- Vec2Dtm(vec = nih_sample$ABSTRACT_TEXT
#'                docnames = nih_sample$APPLICATION_ID, 
#'                min.n.gram = 1, max.n.gram = 2)
#' 
#' dim(dtm)
#' 
#' head(colnames(dtm))
#' 
#' head(rownames(dtm))
#' }
Vec2Dtm <- function(vec, docnames = names(vec), min.n.gram=1, max.n.gram=1, 
                    remove.stopwords=TRUE, custom.stopwords=NULL, lower=TRUE, 
                    remove.punctuation=TRUE, remove.numbers=TRUE, stem.document=FALSE){
  # for now, it is strongly advised to accept the defaults for lower, remove.punctuation, and remove.numbers
  # Other functions are built assuming that the column headings of a dtm contain only letters and underscores "_"
  
  if(is.null(docnames)){
    warning("No document names detected. Assigning 1:length(vec) as names.")
    docnames <- 1:length(vec)
  }  
  
  options(mc.cores = parallel::detectCores())
  
  if(remove.stopwords){
    stopwords <- unique(c(tm::stopwords("english"), tm::stopwords("SMART")))
  }else{
    stopwords <- c()
  }
  
  if(is.null(names(vec))){
    docnames <- 1:length(vec)
  }else{
    docnames <- names(vec)
  }
  
  if( ! is.null(custom.stopwords) ) stopwords <- c(stopwords, custom.stopwords)
  
  if( lower ) {
    vec <- tolower(vec)
    stopwords <- tolower(stopwords)
  }
  
  if( remove.punctuation ){ 
    vec <- gsub("[^a-zA-Z0-9]", " ", vec)
    stopwords <- gsub("[^a-zA-Z0-9]", " ", stopwords)
    stopwords <- unique(unlist(strsplit(stopwords, split="\\s+")))
  }
  
  if( remove.numbers ){ 
    vec <- gsub("[0-9]", " ", vec)
  }
  
  vec <- gsub("\\s+", " ", vec) # remove extra spaces
  
  corp <- tm::Corpus(tm::VectorSource(vec))
  
  
  if( remove.stopwords | ! is.null(custom.stopwords)){
    corp <- tm::tm_map(x=corp, tm::removeWords, stopwords)
  }
  
  if( stem.document ){
    corp <- tm::tm_map(x=corp, tm::stemDocument)
  }
  
  if(max.n.gram == 1){
    dtm <- tm::DocumentTermMatrix(corp)
  }else{
    options(mc.cores=1)
    dtm <- tm::DocumentTermMatrix(corp, control=list(
      tokenize=NgramTokenizer(min=min.n.gram, max=max.n.gram)))
  }
  
  dtm <- MakeSparseDTM(dtm=dtm)
  
  colnames(dtm) <- gsub(" ", "_", colnames(dtm))
  
  rownames(dtm) <- docnames
  
  return(dtm)
}