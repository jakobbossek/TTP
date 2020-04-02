#' @title Edge overlap similarity.
#'
#' @description Given two permutations of \eqn{\{1, \ldots, n\}} the function
#' calculates the number of common edges.
#'
#' @param tour1 [\code{numeric}]\cr
#'   First permutation.
#' @param tour2 [\code{numeric}]\cr
#'   Second permutation.
#' @param normalize [\code{logical(1)}]\cr
#'   Should the number be normalized?
#' @param method [\code{character(1)}]\cr
#'   Method used to measure disorder/dissimilarity One of \dQuote{uncommonedges},
#'   \dQuote{inversion}, \dQuote{maxdist} or \dQuote{run}.
#' @return [\code{numeric(1)} | \code{list}]
#' @rdname permutation disorder
#' @export
measureDisorder = function(tour1, tour2, normalize = FALSE, method = "uncommonedges") {
  checkmate::assertFlag(normalize)
  checkmate::assertChoice(method, choices = c("uncommonedges", "inversion", "run", "maxdist"))
  n = length(tour1)
  set = seq_len(n)
  if (!re::is.permutation(tour1, set) | !re::is.permutation(tour2, set))
    re::stopf("[measureDisorder] tour1 and tour2 need to be permutations of nodes {1, ..., %i}.", n)
  switch(
    method,
    "uncommonedges" = getUncommonEdges(tour1, tour2, normalize),
    # NOTE: order(tour2) is crucial here!
    "inversion" = getNumberOfInversionsC(tour1, order(tour2), normalize),
    "maxdist" = getMaximumDistanceC(tour1, order(tour2), normalize),
    "run" = getRunsC(tour1, order(tour2), normalize)
  ) # switch
}

# measureDisorderSet = function(tour1, tour2, type) {
#   methods = c("uncommonedges", "inversion", "run", "maxdist")
#   measures = lapply(methods, function(m) {
#     measureDisorder(tour1, tour2, method = m, normalize = FALSE)
#   })
#   names(measures) = paste0(type, ".", methods)
#   measures.norm = lapply(methods, function(m) {
#     measureDisorder(tour1, tour2, method = m, normalize = TRUE)
#   })
#   names(measures.norm) = paste0(type, ".", methods, ".norm")
#   return(c(measures, measures.norm))
# }
