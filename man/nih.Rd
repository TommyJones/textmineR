\name{nih}
\docType{data}
\alias{nih}
\alias{nih_sample}
\alias{nih_sample_dtm}
\title{Abstracts and metadata from NIH research grants awarded in 2014}
\description{
  This dataset holds information on research grants awarded by the National
  Institutes of Health (NIH) in 2014. The data set was downloaded in approximately
  January of 2015 from \link{http://exporter.nih.gov/ExPORTER_Catalog.aspx}. It
  includes both 'projects' and 'abstracts' files.
}
\usage{
  data("nih_sample")
  data("nih_dtm")
  }
\format{
  A \code{data.frame} of 100 randomly-sampled grants' abstracts and metadata.
  A \code{dgCMatrix} representing the document term matrix of abstracts from
    100 randomly-sampled grants.
}
\source{National Institutes of Health http://exporter.nih.gov/ExPORTER_Catalog.aspx}

\keyword{datasets}
