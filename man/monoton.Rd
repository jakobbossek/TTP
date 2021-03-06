% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{monoton}
\alias{monoton}
\title{Check for monotonic behavior.}
\usage{
monoton(x, relation = "<=")
}
\arguments{
\item{x}{[\code{numeric}]\cr
Numeric input vector.}

\item{relation}{[\code{string}]\cr
One of \dQuote{<=}, \dQuote{<}, \dQuote{>=} or \dQuote{>}.}
}
\value{
[\code{logical(1)}]
}
\description{
Given a numeric input vector \code{x} and a binary relation
\code{relation}, e.g. \code{relation = "<="}, the function returns a logical
vector \code{y} of length \code{length(x) - 1} with \code{y[i] == TRUE} iff
\code{relation(x[i], x[i+1]) == TRUE} for \eqn{i = 1, \ldots, n-1} where
\eqn{n} is equal to \code{length(x)}.
}
