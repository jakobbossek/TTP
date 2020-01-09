library(ggplot2)
library(tidyverse)
library(scales)

devtools::load_all()

# instance = "instances/eil101-ttp/eil101_n1000_uncorr_01.ttp"
# packing = as.integer(sample(c(0, 1), 1000, replace = TRUE))
# tours = NULL#matrix(sample(1:101) - 1, nrow = 1L)

# st = system.time({
#   res = runTTPSolver(instance, packing, tours, mu = 1L, mutation = "swap", max.evals = 1000000)
# })
# print(res)
# plot(res$trajectory)

# stop()


#' @title Wrapper for runTTPSolver taking care of dynamic changes.
#'
#' @param pathToInstance [\code{character(1)}]\cr
#'   Path to instance.
#' @param pathToPackings [\code{character(1)}]\cr
#'   Path to file with row-wise binary packing vector.
#' @param tau [\code{integer(1)}]\cr
#'   Dynamic change in packing is triggered every \code{tau} function evaluations.
#' @param max.evals.initial [\code{integer(1)}]\cr
#'   Number of evaluations for first initial run.
#' @param max.evals [\code{integer(1) | \code{NULL}}]\cr
#'   Number of evaluations used after each dynamic change. Default is \code{NULL}.
#'   In this case we have \code{max.evals = tau}.
#' @param n.changes [\code{integer(1)}]\cr
#'   Number of dynamic changes.
#' @param full [\code{logical}]\cr
#'   If \code{TRUE} a list of length \code{n.changes+1} is returned with components
#'   \dQuote{tour}, \dQuote{tour.length}, \dQuote{toursFinal} and \dQuote{trajectory}.
#'   If \code{FALSE} - the default - only a length-\code{n.changes + 1} numeric vector
#'   of tour length is returned.
#' @return [\code{list} | \code{numeric}] See \code{full} for details
dynamicWrapper = function(pathToInstance, pathToPackings,
  mutation, mu,
  tau, max.evals.initial,
  max.evals = NULL,
  n.changes = 3L,
  full = FALSE) {

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
    res.run = runTTPSolver(pathToInstance, packing = packings[r, ],
      tours = init.tours, mu = mu, mutation = mutation,
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

# load packings and run
pathToPackings = "packings/BenchmarkTTP_n100_p0.5/BenchmarkTTP_n100_p0.5_c10_1.txt"
pathToInstance = "instances/eil101-ttp/eil101_n1000_uncorr_01.ttp"
max.evals.initial = 10000L
tau = 1000L
n.runs = 30L
res = dynamicWrapper(
  pathToInstance, pathToPackings,
  mutation = 1L, mu = 1L, n.changes = n.runs, max.evals = max.evals.initial,
  tau = tau, max.evals.initial = max.evals.initial, full = TRUE)

#
traj = lapply(res, function(r) r$trajectory)
traj = do.call(c, traj)
plot(traj, type = "s")
abline(v = c(max.evals.initial + (0:n.runs) * tau), col = scales::alpha("blue", 0.12), lwd = 2, lty = 3)
#res = runTTPSolver(instance, packing = packing, initial.tour = initial.tour, max.iter = 10000)
#print(res)
