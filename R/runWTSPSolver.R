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
#' @param survival.strategy [\code{character(1)}]\cr
#'   How to perform survival selection. Option \dQuote{classic} drops the
#'   worst out of \eqn{\mu + 1} individuals while option \dQuote{parent} compares
#'   the fitness of the child with its parent fitness only.
#'   Note that for \code{mu = 1} both strategies behave in the same way.
#'   Default is \code{classic}.
#' @param mutation [\code{character(1)}]\cr
#'   One of \dQuote{swap}, \dQuote{jump}, \dQuote{inversion} or \dQuote{scramble}.
#' @param max.evals [\code{integer}]\cr
#'   Maximum number of function evaluations.
#' @return [\code{list}]
#' @export
runWTSPSolver = function(instance, packing, tours = NULL, mu = 1L, mutation = "swap", survival.strategy = "classic", max.evals) {
  checkmate::assertFileExists(instance)
  checkmate::assertInteger(packing, lower = 0, upper = 1, any.missing = FALSE, all.missing = FALSE)
  checkmate::assertMatrix(tours, nrows = mu, null.ok = TRUE)
  mu = checkmate::asInt(mu)
  checkmate::assertChoice(mutation, choices = c("swap", "inversion", "jump", "scramble"))
  checkmate::assertChoice(survival.strategy, choices = c("classic", "parent"))
  max.evals = checkmate::asInt(max.evals)

  dimline = readLines(instance, 3)[3L]
  n = as.integer(strsplit(dimline, split = "[[:space:]]*:[[:space:]]*")[[1]][2])

  mutation.mapping = c("swap" = 0, "jump" = 1, "inversion" = 2, "scramble" = 3)#, "2opt" = 4)
  survival.mapping = c("classic" = 0, "parent" = 1)
  mutation = mutation.mapping[mutation]
  survival.strategy = survival.mapping[survival.strategy]

  if (is.null(tours)) {
    tours = lapply(seq_len(mu), function(i) {
      sample(1:n)
    })
    tours = do.call(rbind, tours)
  }

  # delegate to c++
  runWTSPSolverC(instance, packing, tours, mu, mutation, survival.strategy, max.evals)
}
