#' @title Tour where nodes are visited by increasing weight.
#'
#' @description For the node-weight dependent TSP (WTSP) in particular and
#' for the more realistic multi-component traveling thief problem (TTP) it
#' is reasonable to think of heavy-weights nodes being visited late in the
#' optimal tour. This function calculates the extreme setting. I.e. starting
#' in node 1 the remaining nodes are visited in increasing order of their
#' node weight, i.e. the sum of weights of packed items.
#'
#' @param prob [\code{ttp_instance}]\cr
#'   TTP instance.
#' @param packing [\code{integer}]\cr
#'   Packing vector.
#' @return [\code{integer}] Permutation of nodes.
#' @export
getTourByIncreasingWeight = function(prob, packing) {
  checkmate::assertClass(prob, "ttp_instance")
  checkmate::assertIntegerish(packing, len = prob$m, lower = 0, upper = 1, any.missing = FALSE, all.missing = FALSE)

  # get weights per node
  wbn = getWeightByNodeC(prob, packing, prob$items$weight, prob$items$nodenr)

  # now form tour: 1st node is 1 and the following are the remaining nodes
  # in their order of weight. Note that +1 here is necessary!
  tour = c(1, order(wbn[-1L], decreasing = FALSE) + 1L)
  stopifnot(re::is.permutation(tour, s = 1:prob$n))
  #print(tour)
  #print(wbn[tour])
  return(tour)
}
