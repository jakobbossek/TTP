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
#'   If \code{NULL}, all items are packed.
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
#' @param objective.type [\code{character(1)}]\cr
#'   Which objective function should be used?
#'   One of \dQuote{wtsp} (node weighted TSP) or \dQuote{ttp} (Traveling Thief Problem).
#' @param max.evals [\code{integer}]\cr
#'   Maximum number of function evaluations.
#' @return [\code{list}]
#' @rdname runWTSPSolver
#' @name runWTSPSolver
#' @export
runWTSPSolver = function(instance, packing = NULL, tours = NULL, mu = 1L, mutation = "swap", objective.type = "wtsp", survival.strategy = "classic", max.evals) {
  checkmate::assertFileExists(instance)
  checkmate::assertInteger(packing, lower = 0, upper = 1, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  checkmate::assertMatrix(tours, nrows = mu, null.ok = TRUE)
  mu = checkmate::asInt(mu)
  checkmate::assertChoice(mutation, choices = c("swap", "inversion", "jump", "scramble"))
  checkmate::assertChoice(survival.strategy, choices = c("classic", "parent"))
  checkmate::assertChoice(objective.type, choices = c("wtsp", "ttp"))
  max.evals = checkmate::asInt(max.evals)

  dimline = readLines(instance, 3)[3L]
  n = as.integer(strsplit(dimline, split = "[[:space:]]*:[[:space:]]*")[[1]][2])

  itemsline = readLines(instance, 4)[4L]
  m = as.integer(strsplit(itemsline, split = "[[:space:]]*:[[:space:]]*")[[1]][2])

  mutation.mapping = c("swap" = 0, "jump" = 1, "inversion" = 2, "scramble" = 3)#, "2opt" = 4)
  survival.mapping = c("classic" = 0, "parent" = 1)
  objective.mapping = c("wtsp" = 0, "ttp" = 1)
  mutation = mutation.mapping[mutation]
  survival.strategy = survival.mapping[survival.strategy]
  objective.type = objective.mapping[objective.type]

  if (is.null(tours)) {
    tours = lapply(seq_len(mu), function(i) {
      sample(1:n)
    })
    tours = do.call(rbind, tours)
  }

  if (is.null(packing)) {
    packing = rep(1, m)
  }

  # delegate to c++
  # switch(
  #   language,
  #   "cpp" = runWTSPSolverC(instance, packing, tours, mu, mutation, objective.type, survival.strategy, max.evals),
  #   "r"   = runWTSPSolverR(instance, packing, tours, mu, mutation, objective.type, survival.strategy, max.evals)
  # ) # switch
  runWTSPSolverC(instance, packing, tours, mu, mutation, objective.type, survival.strategy, max.evals)
}

#' @rdname runWTSPSolver
#' @name runWTSPSolver
#' @export
runWTSPSolverR = function(instance, packing = NULL, tours = NULL, mu = 1L, mutation = "swap", objective.type = "wtsp", survival.strategy = "classic", max.evals) {
  checkmate::assertFileExists(instance)
  checkmate::assertInteger(packing, lower = 0, upper = 1, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  checkmate::assertMatrix(tours, nrows = mu, null.ok = TRUE)
  mu = checkmate::asInt(mu)
  checkmate::assertChoice(mutation, choices = c("swap", "inversion", "jump", "scramble"))
  checkmate::assertChoice(survival.strategy, choices = c("classic", "parent"))
  checkmate::assertChoice(objective.type, choices = c("wtsp", "ttp", "tsp"))
  max.evals = checkmate::asInt(max.evals)

  fitness.fun = switch(
    objective.type,
    "tsp"  = tsp,
    "ttp"  = ttp,
    "wtsp" = wtsp
  ) # switch

  # load instance
  prob = load(instance)

  # init population ...
  if (is.null(tours)) {
    tours = lapply(seq_len(mu), function(i) {
      sample(1:prob$n)
    })
    tours = do.call(rbind, tours)
  }

  # ... and packing (if not given)
  if (is.null(packing)) {
    packing = rep(1, prob$m)
  }

  fun       = if (objective.type == "ttp") base::max else base::min
  which.fun = if (objective.type == "ttp") base::which.max else base::which.min
  comp.fun  = if (objective.type == "ttp") match.fun(FUN = ">=") else match.fun(FUN = "<=")
  comp.fun.strict = if (objective.type == "ttp") match.fun(FUN = ">") else match.fun(FUN = "<")
  mut.fun   = switch(
    mutation,
    "swap" = ecr::mutSwap,
    "inversion" = ecr::mutInversion,
    "jump" = stopf("[runWTSPSolverR] Mutation 'jump' not yet implemented in ecr."),
    "scramble" = stopf("[runWTSPSolverR] Mutation 'scramble' not yet implemented in ecr.")
  ) # switch

  n.iters = 0L
  n.evals = 0L
  trajectory = numeric(max.evals)

  fitness = apply(tours, 1L, fitness.fun, prob = prob, packing = packing)
  best.idx = which.fun(fitness)
  best.tour = tours[best.idx, ]
  best.fitness = fitness[best.idx]
  n.evals = n.evals + mu
  trajectory[seq_len(mu)] = best.fitness

  repeat {
    # sample parent
    parent.idx = sample(seq_len(mu), size = 1L)
    parent = tours[parent.idx, ]

    # generate child
    child = mut.fun(parent)
    child.fitness = fitness.fun(child, prob, packing)

    found.better = comp.fun.strict(child.fitness, best.fitness)

    # select best
    if (survival.strategy == "parent") {
      # very simple diversity preservation: compare with parent only
      if (comp.fun(child.fitness, parent)) {
        tours[parent.idx, ] = child
        fitness[parent.idx] = child.fitness
      }
    } else {
      # classic (mu + 1)-EA
      if (comp.fun(child.fitness, best.fitness)) {
        tours[best.idx, ] = child
        fitness[best.idx] = child.fitness
      }
    }

    n.evals = n.evals + 1L
    n.iters = n.iters + 1L

    # update incumbent
    best.idx = which.fun(fitness)
    best.tour = tours[best.idx, ]
    best.fitness = fitness[best.idx]
    trajectory[n.evals] = best.fitness

    # logging
    if (found.better) {
      catf("New best value at iteration %i is %.2f", n.iters, trajectory[n.evals])
    }

    # check termination condition
    if (n.evals >= max.evals)
      break
  }

  list(
    tour.length = best.fitness,
    tour = best.tour,
    tour.length.wtsp = wtsp(best.tour, prob, packing),
    tour.length.ttp  = ttp(best.tour, prob, packing),
    finalTours = tours,
    trajectory = trajectory)
}

mutInversion2 = function(ind) {
  n = length(ind)
  positionA = sample(1:n, 1L)
  positionB = sample(1:n, 1L)
  if (positionA != positionB) {
    if (positionB < positionA) {
      tmp = positionA
      positionA = positionB
      positionB = tmp
    }
    i = positionA
    j = positionB
    while (i < j) {
      tmp = ind[i]
      ind[i] = ind[j]
      ind[j] = tmp
      i = i + 1L
      j = j - 1L
    }
  }
  return(ind)

}
