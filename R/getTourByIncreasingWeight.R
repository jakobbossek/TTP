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
  n = prob$n
  D = prob$distance.matrix

  # get weights per node
  wbn = getWeightByNodeC(prob, packing, prob$items$weight, prob$items$nodenr)

  # now form tour: 1st node is 1 and the following are the remaining nodes
  # in their order of weight. Note that +1 here is necessary!
  ord = c(1L, order(wbn[-1L], decreasing = FALSE) + 1L)
  wbnord = wbn[ord]
  # now choose greedily closest node if there are several with the same weight
  tour = c(1, rep(NA, n - 1L))
  i = 2L
  print(wbn)
  print(ord)
  print(wbn[ord])
  while (i <= n) {
    l = i
    u = i
    cw = wbnord[i]
    # find block with all equal weights
    while ((u + 1) <= n) {
      if (wbnord[u+1] == cw) {
        u = u + 1L
      } else {
        break
      }
    }
    #catf("Block length: %i, weight: %i\n", u-l+1, cw)
    # current block has length 1
    if (l == u) {
      # no choice: there is just one node with the current weight
      tour[i] = ord[i]
      i = i + 1L
    } else {
      # there are > 1 nodes with the same weight -> Add these in
      # their order of distance
      p = l
      cands = ord[l:u]
      while (p <= u) {
        source = tour[p-1L]
        dists = D[source, cands]
        idx.min = which.min(dists)
        tour[p] = cands[idx.min]

        cands = setdiff(cands, cands[idx.min])
        p = p + 1L
      }
      i = u + 1L
    }
  }

  #JAKOB: the next line is the simple version (no greedy approach by distance in presence of ties)
  #tour = c(1, order(wbn[-1L], decreasing = FALSE) + 1L)
  stopifnot(re::is.permutation(tour, s = 1:n))
  return(tour)
}
