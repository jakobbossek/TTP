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
