#' @title Wrapper for runWTSPSolver taking care of dynamic changes.
#'
#' @param pathToInstance [\code{character(1)}]\cr
#'   Path to instance.
#' @param pathToPackings [\code{character(1)}]\cr
#'   Path to file with row-wise binary packing vector.
#' @param mutation [\code{character(1)}]\cr
#'   See \code{\link{runWTSPSolver}}.
#' @param objective.type [\code{character(1)}]\cr
#'   Which objective function should be used?
#'   One of \dQuote{wtsp} (node weighted TSP) or \dQuote{ttp} (Traveling Thief Problem).
#' @param mu [\code{integer(1)}]\cr
#'   Population size.
#' @param survival.strategy [\code{character(1)}]\cr
#'   How to perform survival selection. Option \dQuote{classic} drops the
#'   worst out of \eqn{\mu + 1} individuals while option \dQuote{parent} compares
#'   the fitness of the child with its parent fitness only.
#'   Note that for \code{mu = 1} both strategies behave in the same way.
#'   Default is \code{classic}.
#' @param tau [\code{integer(1)}]\cr
#'   Dynamic change in packing is triggered every \code{tau} function evaluations.
#' @param max.evals.initial [\code{integer(1)}]\cr
#'   Number of evaluations for first initial run.
#' @param max.evals [\code{integer(1)} | \code{NULL}]\cr
#'   Number of evaluations used after each dynamic change. Default is \code{NULL}.
#'   In this case we have \code{max.evals = tau}.
#' @param n.changes [\code{integer(1)}]\cr
#'   Number of dynamic changes.
#' @param start.from.scratch [\code{logical(1)}]\cr
#'   Start with random population each time a dynamic change occurs?
#'   Default is \code{FALSE}.
#' @param full [\code{logical(1)}]\cr
#'   If \code{TRUE} a list of length \code{n.changes + 1} is returned with components
#'   \dQuote{tour}, \dQuote{tour.length}, \dQuote{toursFinal} and \dQuote{trajectory}.
#'   If \code{FALSE} - the default - only a length-\code{n.changes + 1} numeric vector
#'   of tour length is returned.
#' @return [\code{list} | \code{numeric}] See \code{full} for details
#' @export
runWTSPSolverInDynamicSetting = function(
  pathToInstance, pathToPackings,
  mutation, objective.type = "wtsp",
  mu, survival.strategy,
  tau,
  max.evals.initial,
  max.evals = NULL,
  n.changes = 3L,
  start.from.scratch = FALSE,
  full = FALSE) {
  checkmate::assertFileExists(pathToInstance)
  checkmate::assertFileExists(pathToPackings)
  checkmate::assertChoice(mutation, choices = c("swap", "inversion", "jump", "scramble"))
  checkmate::assertChoice(objective.type, choices = c("wtsp", "ttp"))
  checkmate::assertChoice(survival.strategy, choices = c("classic", "parent"))
  mu = checkmate::asInt(mu)
  tau = checkmate::asInt(tau)
  max.evals.initial = checkmate::asInt(max.evals.initial)
  checkmate::assertInt(max.evals, null.ok = TRUE)
  checkmate::assertInt(n.changes, lower = 1L)
  checkmate::assertFlag(start.from.scratch)
  checkmate::assertFlag(full)

  # inport
  n.runs = n.changes + 1L # plus initial run
  res = vector(mode = "list", length = n.runs)

  packings = as.matrix(read.table(pathToPackings, header = FALSE, sep = " "))
  if (nrow(packings) < n.runs) {
    BBmisc::stopf("[dynamicWrapper] Number of packings (%i) is lower than the number of runs (%i).",
      nrow(packings), n.runs)
  }

  do.dynamic = is.null(max.evals)

  init.tours = NULL
  for (r in seq_len(n.runs)) {
    if (do.dynamic) {
      max.evals = if (r == 1L) max.evals.initial else tau
    }
    BBmisc::catf("Run %i", r - 1L)
    res.run = runWTSPSolverR(pathToInstance, packing = packings[r, ],
      tours = if (!start.from.scratch) init.tours else NULL,
      mu = mu, mutation = mutation, objective.type = objective.type,
      survival.strategy = survival.strategy,
      max.evals = max.evals)
    init.tours = res.run$finalTours
    if (!full) {
      res.run$trajectory = res.run$finalTours = res$tour = NULL
      res[[r]] = res.run$tour.length
    } else {
      res[[r]] = res.run
    }
  }

  if (!full)
    res = unname(unlist(res))

  return(res)
}
