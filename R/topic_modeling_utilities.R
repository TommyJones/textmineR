#' Summarize topics in a topic model
#' @description Create a data frame summarizing the contents of each topic in a
#' model
#' @param model A list (or S3 object) with three named matrices: phi, theta,
#' and gamma.
#'        These conform to outputs of many of \link[textmineR]{textmineR}'s
#' native
#'        topic modeling functions such as \link[textmineR]{FitLdaModel}.
#' @return An object of class \code{data.frame} or \code{tibble} with 6 columns:
#' 'topic' is the
#'         name of the topic, 'prevalence' is the rough prevalence of the topic
#'         in all documents across the corpus, 'coherence' is the probabilistic
#'         coherence of the topic, 'top_terms_phi' are the top 5 terms for each
#'         topic according to P(word|topic), 'top_terms_gamma' are the top 5
#' terms
#'         for each topic according to P(topic|word).
#' @details 'prevalence' is normalized to sum to 100. If your 'theta' matrix has
#'          negative values (as may be the case with an LSA model), a constant
#' is
#'          added so that the least prevalent topic has a prevalence of 0.
#'
#'          'coherence' is calculated using \link[textmineR]{CalcProbCoherence}.
#'
#'          'label' is assigned using the top label from
#' \link[textmineR]{LabelTopics}.
#'          This requires an "assignment" matrix. This matrix is like a "theta"
#' matrix
#'          except that it is binary. A topic is "in" a document or it is not.
#'          The assignment is made by comparing each value of theta to the
#' minimum
#'          of the largest value for each row of theta (each document). This
#'          ensures that each document has at least one topic assigned to it.
#' @example examples/summarize_topics.R
#' @export
summarize_topics <- function(model) {

  # check inputs
  if (!"phi" %in% names(model) | !"theta" %in% names(model)) {
      stop(
          "model must contain a 'phi' matrix and a 'theta'",
          "matrix named as such."
      )
  }

  # get coherence
  if ("coherence" %in% names(model)) {
    coherence <- model$coherence
  } else {
    coherence <- textmineR::calc_prob_coherence(model$phi, model$data)
  }

  # get prevalence - this gets fancy to account for negatives in LSA models
  p <- colSums(model$theta)

  if (sum(p < 0) > 0) { # if there are any negatives
    p <- p + min(p)
  }

  prevalence <- p / sum(p) * 100

  # get top terms from phi
  tt_phi <- get_top_terms(phi = model$phi, M = 5)

  tt_phi <- apply(tt_phi, 2, function(x) {
    paste(x, collapse = ", ")
  })

  # get top terms from gamma
  tt_gamma <- get_top_terms(phi = model$gamma, M = 5)

  tt_gamma <- apply(tt_gamma, 2, function(x) {
    paste(x, collapse = ", ")
  })

  # get labels
  m <- apply(model$theta, 1, max, na.rm = TRUE)

  m <- min(m, na.rm = TRUE)

  a <- model$theta >= m

  labels <- label_topics(a, model$data, M = 1)

  # prepare output
  out <- data.frame(
    topic = rownames(model$phi),
    label = labels,
    prevalence = round(prevalence, 2),
    coherence = round(coherence, 3),
    top_terms_phi = tt_phi,
    top_terms_gamma = tt_gamma,
    stringsAsFactors = FALSE
  )

  if ("tibble" %in% row.names(utils::installed.packages())) {
    out <- tibble::as_tibble(out)
  }

  out
}

#' @rdname summarize_topics
#' @export
summarise_topics <- summarize_topics # for the brits out there

#' @rdname summarize_topics
#' @export
SummarizeTopics <- summarize_topics

#' Get cluster labels using a "more probable" method of terms
#'
#' @description Function extracts probable terms from a set of documents.
#' Probable here implies more probable than in a corpus overall.
#' @param docnames A character vector of rownames of dtm for set of documents
#' @param dtm A document term matrix of class \code{matrix} or \code{dgCMatrix}.
#' @param p_terms If not NULL (the default), a numeric vector representing the
#' probability of each term in the corpus whose names correspond to
#' colnames(dtm).
#' @return
#' Returns a numeric vector of the format p_terms. The entries of the vectors
#' correspond to the difference in the probability of drawing a term from the
#' set of documents given by docnames and the probability of drawing that term
#' from the corpus overall (p_terms).
#'
#' @export
#' @example examples/get_probable_terms.R
get_probable_terms <- function(docnames, dtm, p_terms = NULL) {

  # if p_terms is NULL, then create p_terms
  if (is.null(p_terms)) {
    p_terms <- Matrix::colSums(dtm) / sum(Matrix::colSums(dtm))
  }

  # get probability of terms given docnames
  if (length(docnames) == 1) {
    p_terms_given_docs <- dtm[docnames, ]
  } else {
    p_terms_given_docs <- Matrix::colSums(dtm[docnames, ])
  }

  p_terms_given_docs <- p_terms_given_docs / sum(p_terms_given_docs)

  # get our result, the difference
  result <- p_terms_given_docs - p_terms

  names(result) <- colnames(dtm)

  return(result)
}

#' @rdname get_probable_terms
#' @param ... arguments to be passed to \code{get_probable_terms}
#' @export
GetProbableTerms <- function(...) {
    .Deprecated(
        new = "get_probable_terms",
        package = "textmineR"
    )
    get_probable_terms(...)
}

#' Get some topic labels using a "more probable" method of terms
#'
#' @description Function calls \code{\link[textmineR]{GetProbableTerms}} with
#' some rules to get topic labels. This function is in "super-ultra-mega alpha";
#' use at your own risk/discretion.
#' @param assignments A documents by topics matrix similar to \code{theta}.
#' This will work best if this matrix is sparse, with only a few non-zero topics
#' per document.
#' @param dtm A document term matrix of class \code{matrix} or \code{dgCMatrix}.
#' The columns of \code{dtm} should be n-grams whose colnames have a "_" where
#' spaces would be between the words.
#' @param M The number of n-gram labels you want to return. Defaults to 2
#' @return Returns a \code{matrix} whose rows correspond to topics and whose
#' j-th column corresponds to the j-th "best" label assignment.
#' @export
#' @example examples/label_topics.R
label_topics <- function(assignments, dtm, M = 2) {
  # figure out a threshold
  threshold <- apply(assignments, 2, function(x) max(x, na.rm = T))
  threshold <- min(threshold[threshold > 0 & !is.na(threshold)])

  # get a list of documents for each topic
  doc_list <- apply(assignments, 2, function(x) {
    names(x)[x >= threshold]
  })

  # get dtm_ngram and p_terms
  dtm_ngram <- dtm[, grepl("_", colnames(dtm))]

  if (ncol(dtm_ngram) == 0) {
    message(
      "dtm does not appear to contain ngrams. Using unigrams but ngrams will",
      " work much better."
    )
    dtm_ngram <- dtm
  }

  p_terms <- Matrix::colSums(dtm_ngram)
  p_terms <- p_terms / sum(p_terms)

  # apply the label algorithm over each topic
  result <- lapply(doc_list, function(x) {
    l <- get_probable_terms(docnames = x, dtm = dtm_ngram, p_terms = p_terms)
    names(l)[order(l, decreasing = T)][1:M]
  })

  # format into a matrix for output
  result <- do.call(rbind, result)

  colnames(result) <- paste("label_", seq_len(ncol(result)), sep = "")

  result
}

#' @rdname label_topics
#' @export
LabelTopics <- label_topics

#' Get Top Terms for each topic from a topic model
#'
#' @description Takes topics by terms matrix and returns top M terms for each
#' topic
#' @param phi A matrix whose rows index topics and columns index words
#' @param M An integer for the number of terms to return
#' @param return_matrix Do you want a \code{matrix} or \code{data.frame}/
#' \code{tibble} returned? Defaults to \code{TRUE}.
#' @return
#' If \code{return_matrix = TRUE} (the default) then a matrix. Otherwise,
#' returns a \code{data.frame} or \code{tibble} whose columns correspond to a
#' topic and whose m-th row correspond to the m-th top term from the input
#' \code{phi}.
#' @export
#' @example examples/get_top_terms.R
get_top_terms <- function(phi, M, return_matrix = TRUE) {
  result <- apply(phi, 1, function(x) {
    names(x)[order(x, decreasing = TRUE)][1:M]
  })

  if (!return_matrix) {
    if ("tibble" %in% row.names(utils::installed.packages())) {
      result <- tibble::as_tibble(result)
    } else {
      result <- as.data.frame(result)
    }
  }

  return(result)
}

#' @rdname get_top_terms
#' @export
GetTopTerms <- get_top_terms
