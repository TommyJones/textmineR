\name{acq2}
\docType{data}
\alias{acq2}
\alias{acq}
\alias{documents}
\alias{dtm}
\alias{model}
\title{50 Exemplary News Articles from the Reuters-21578 Data Set of Topic acq}
\description{
  This dataset holds 50 news articles with additional meta information from the
  Reuters-21578 data set. All documents belong to the topic \code{acq2} dealing
  with corporate acquisitions. The data come in several formats: a character
  vector, a sparse document term matrix, and a topic model. The source data are 
  also available in the \code{tm} package by calling \code{data(acq)}.
}
\usage{data("acq2")}
\format{A \code{\link{VCorpus}} of 50 text documents, A character vector of
        the same data, a \code{dgCMatrix}, a topic model as a \code{list}.}
\source{Reuters-21578 Text Categorization Collection Distribution 1.0
  (\acronym{XML} format).}
\references{
  Lewis, David (1997) \emph{Reuters-21578 Text Categorization
    Collection Distribution 1.0}.
  \url{http://kdd.ics.uci.edu/databases/reuters21578/reuters21578.html}

  Luz, Saturnino \emph{\acronym{XML}-encoded version of Reuters-21578}.
  \url{http://ronaldo.cs.tcd.ie/esslli07/data/reuters21578-xml/}
}
\examples{
data("acq2")
str(acq)
str(documents)
str(dtm)
str(model)
}
\keyword{datasets}
