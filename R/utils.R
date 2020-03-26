#' @title Normalize a numeric vector.
#'
#' @description After normalization, all values in \code{normalize(x)} are
#' in \eqn{[0, 1]}.
#'
#' @param x [\code{numeric}]\cr
#'   Numeric input vector.
#' @return [\code{numeric}] Normalized \code{x}.
#' @export
normalize = function(x) {
  checkmate::assertNumeric(x, min.len = 2L, any.missing = FALSE, all.missing = FALSE)
  (x - min(x)) / (max(x) - min(x))
}

#' @title Check for monotonic behavior.
#'
#' @description Given a numeric input vector \code{x} and a binary relation
#' \code{relation}, e.g. \code{relation = "<=""}, the function returns a logical
#' vector \code{y} of length \code{length(x) - 1} with \code{y[i] == TRUE} iff
#' \code{relation(x[i], x[i+1]) == TRUE} for \eqn{i = 1, \ldots, n-1} where
#' \eqn{n} is equal to \code{length(x)}.
#'
#' @param x [\code{numeric}]\cr
#'   Numeric input vector.
#' @param relation [\code{string}]\cr
#'   One of \dQuote{<=}, \dQuote{<}, \dQuote{>=} or \dQuote{>}.
#' @return [\code{logical(1)}]
#' @export
monoton = function(x, relation = "<=") {
  checkmate::assertNumeric(x, min.len = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertSubset(relation, choices = c("<=", "<", ">=", ">"))
  relation = base::match.fun(relation)
  n = length(x)
  relation(x[1L:(n-1L)], x[2L:n])
}

#' @title Get monotonic sequences.
#'
#' @description Given a numeric input vector \code{x} and a binary relation
#' \code{relation} the function returns a list with three componentes indicating
#' the start and end positions of monotonic blocks in \code{x} and its lengths.
#'
#' @inheritParams monoton
#' @return [\code{matrix(2, *)}] Matrix with start (top row) and end (bottom row)
#' indizes.
#' @export
getMonotonicBlocks = function(x, relation = "<=") {
  checkmate::assertNumeric(x, min.len = 2L, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertSubset(relation, choices = c("<=", "<", ">=", ">"))
  mon = monoton(x, relation)
  # +1 in the following because we compare x[i] with x[i+1] and hence
  # vector 'mon' has length (length(x) - 1)
  getMonotonicBlocksC(mon)
}
