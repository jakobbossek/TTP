% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{getMonotonicBlocks}
\alias{getMonotonicBlocks}
\title{Get monotonic sequences.}
\usage{
getMonotonicBlocks(x, relation = "<=")
}
\arguments{
\item{x}{[\code{numeric}]\cr
Numeric input vector.}

\item{relation}{[\code{string}]\cr
One of \dQuote{<=}, \dQuote{<}, \dQuote{>=} or \dQuote{>}.}
}
\value{
[\code{matrix(2, n)}] Matrix with start (top row) and end (bottom row)
indizes.
}
\description{
Given a numeric input vector \code{x} and a binary relation
\code{relation} the function returns a list with three componentes indicating
the start and end positions of monotonic blocks in \code{x} and its lengths.
}
