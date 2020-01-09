#' @title Run weighted TSP solver.
#'
#' @description Given a problem instance and a binary packing plan, i.e. with \code{packing[i] == 1}
#' meaning that the i-th element must be taken, calculate a minimum length node-weighted
#' TSP tour.
#'
#' @param instance [\code{character(1)}]\cr
#'   Path to instance.
#' @param packing [\code{integer}]\cr
#'   Binary vector of packed items.
#' @param tours [\code{matrix}]\cr
#'   A \code{mu} times \eqn{n} matrix where \code{mu} is the population size and
#'   \code{n} is the number of nodes. Each row is one initial TSP tour.
#'   If \code{NULL}, random permutation are generated.
#' @param mu [\code{integer(1)}]\cr
#'   Population size.
#' @param mutation [\code{character(1)}]\cr
#'   One of \dQuote{swap}, \dQuote{jump}, \dQuote{inversion} or \dQuote{scramble}.
#' @param max.evals [\code{integer}]\cr
#'   Maximum number of function evaluations.
#' @return [\code{list}]
#' @export
runTTPSolver = function(instance, packing, tours = NULL, mu = 1L, mutation = "swap", max.evals) {
  dimline = readLines(instance, 3)[3L]
  n = as.integer(strsplit(dimline, split = "[[:space:]]*:[[:space:]]*")[[1]][2])

  mutation.mapping = c("swap" = 0, "jump" = 1, "inversion" = 2, "scramble" = 3)#, "2opt" = 4)
  mutation = mutation.mapping[mutation]

  if (is.null(tours)) {
    tours = lapply(seq_len(mu), function(i) {
      sample(1:n) - 1L # 0-based in c++
    })
    tours = do.call(rbind, tours)
  }

  # delegate to c++
  runTTPSolverC(instance, packing, tours, mu, mutation, max.evals)
}
