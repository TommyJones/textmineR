#' textmineR
#'
#' Functions for Text Mining and Topic Modeling
#'
#' An aid for text mining in R, with a syntax that
#' should be familiar to experienced R users. Provides a wrapper for several 
#' topic models that take similarly-formatted input and give similarly-formatted
#' output. Has additional functionality for analyzing and diagnostics for
#' topic models.
#'
#' @name textmineR
#' @docType package
NULL

#' @import Matrix
#' @import Rcpp
#' @importFrom Rcpp sourceCpp
#' @import RcppProgress
#' @importFrom methods as
#' @export CalcLikelihoodC
#' @export CalcSumSquares
#' @export Dtm2DocsC
#' @export Hellinger_cpp
#' @export HellingerMat
#' @export JSD_cpp
#' @export JSDmat
#' @export dtm_to_lexicon_c
#' @export fit_lda_c
#' @export predict_lda_c
#' @useDynLib "textmineR", .registration=TRUE
NULL
