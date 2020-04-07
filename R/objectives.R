#' @title Objectives
#'
#' @description Classical Traveling Salesperson Problem (tsp),
#' node-weighted TSP (wtsp) and single-objective formulation of
#' the Traveling-Thief-Problem (ttp). In addition wttp returns
#' the weight-related component of the TTP only; to be minimized.
#'
#' @param tour [\code{integer}]\cr
#'   Permutation of nodes.
#' @param prob [\code{ttp_instance}]\cr
#'   TTP instance.
#' @param packing [\code{integer}]\cr
#'   Binary vector, i.e. with ones and zeros, of length \code{prob$m}
#'   where each component indicates whether the corresponding item
#'   should be packed (1) or not (0).
#'   This argument is ignored by \code{tsp}.
#' @param ... [any]\cr
#'   Not used at the moment.
#' @return [\code{numeric(1)}] Objective function value.
#' @rdname objectives
#' @name objectives
#' @export
tsp = function(tour, prob, packing, ...) {
  tspC(tour, prob$distance.matrix)
}

#' @rdname objectives
#' @export
wtsp = function(tour, prob, packing, ...) {
  wtspC(tour, prob, packing, prob$items$weight, prob$items$nodenr)
}

#' @rdname objectives
#' @export
ttp = function(tour, prob, packing, ...) {
  ttpC(tour, prob, packing, prob$items$profit, prob$items$weight, prob$items$nodenr, weightOnly = FALSE)
}

#' @rdname objectives
#' @export
wttp = function(tour, prob, packing, ...) {
  ttpC(tour, prob, packing, prob$items$profit, prob$items$weight, prob$items$nodenr, weightOnly = TRUE)
}
