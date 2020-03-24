#' @rdname runWTSPSolver
#' @name runWTSPSolver
#' @export
runWTSPSolverR = function(instance, packing = NULL, tours = NULL, mu = 1L, mutation = "swap", objective.type = "wtsp", survival.strategy = "classic", max.evals, extended.trajectory = FALSE) {
  checkmate::assertFileExists(instance)
  checkmate::assertInteger(packing, lower = 0, upper = 1, any.missing = FALSE, all.missing = FALSE, null.ok = TRUE)
  checkmate::assertMatrix(tours, nrows = mu, null.ok = TRUE)
  mu = checkmate::asInt(mu)
  checkmate::assertChoice(mutation, choices = c("swap", "inversion", "jump", "scramble"))
  checkmate::assertChoice(survival.strategy, choices = c("classic", "parent"))
  checkmate::assertChoice(objective.type, choices = c("wtsp", "ttp", "tsp"))
  max.evals = checkmate::asInt(max.evals)
  checkmate::assertFlag(extended.trajectory)

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
    "jump" = ecr::mutJump,
    "scramble" = ecr::mutScramble
  ) # switch

  n.iters = 0L
  n.evals = 0L
  trajectory = matrix(NA, nrow = max.evals, ncol = 4)

  fitness = apply(tours, 1L, fitness.fun, prob = prob, packing = packing)
  best.idx = which.fun(fitness)
  best.tour = tours[best.idx, ]
  best.fitness = fitness[best.idx]
  n.evals = n.evals + mu
  trajectory[seq_len(mu), 1L] = best.fitness

  if (extended.trajectory) {
    trajectory[seq_len(mu), 2L] = tsp(best.tour, prob, packing)
    trajectory[seq_len(mu), 3L] = wtsp(best.tour, prob, packing)
    trajectory[seq_len(mu), 4L] = ttp(best.tour, prob, packing)
  }

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
    trajectory[n.evals, 1L] = best.fitness

    if (extended.trajectory) {

      trajectory[n.evals, 2L] = tsp(best.tour, prob, packing)
      trajectory[n.evals, 3L] = wtsp(best.tour, prob, packing)
      trajectory[n.evals, 4L] = ttp(best.tour, prob, packing)
    }

    # logging
    if (found.better) {
      catf("New best value at iteration %i is %.2f", n.iters, trajectory[n.evals])
    }

    # check termination condition
    if (n.evals >= max.evals)
      break
  }

  # finalize trajectory
  if (!extended.trajectory) {
    trajectory = trajectory[, 1L, drop = TRUE]
  } else {
    trajectory = as.data.frame(trajectory)
    colnames(trajectory) = c("objective", "tsp", "wtsp", "ttp")
  }

  list(
    tour.length = best.fitness,
    tour = best.tour,
    tour.length.wtsp = wtsp(best.tour, prob, packing),
    tour.length.ttp  = ttp(best.tour, prob, packing),
    finalTours = tours,
    trajectory = trajectory)
}
