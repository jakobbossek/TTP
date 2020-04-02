#' @title Shift tour.
#'
#' @description Shifting is performed such that node 1 is the first in the
#' permutation.
#'
#' @param x [\code{integer}]\cr
#'   Permutation of \eqn{\{1, \ldots, n\}}.
#' @return [\code{integer}] Shifted permutation.
#' @export
shiftTour = function(x) {
  checkmate::assertIntegerish(x, any.missing = FALSE, all.missing = FALSE)
  shiftTourC(x)
}
