library(ggplot2)
library(tidyverse)
library(scales)

devtools::load_all()

# load packings and run
pathToPackings = "packings/BenchmarkTTP_n100_p0.5/BenchmarkTTP_n100_p0.5_c10_1.txt"
pathToInstance = "instances/eil101-ttp/eil101_n1000_uncorr_01.ttp"
max.evals.initial = 10000L
tau = 1000L
n.runs = 10L

# st = system.time({
# res = runWTSPSolver(
#   pathToInstance, as.matrix(read.table(pathToPackings, sep = " ", header = FALSE))[1, ],
#   mutation = "inversion", mu = 20L, max.evals = 1000000)
# })
# print(st)
# stop()


res = runWTSPSolverInDynamicSetting(
  pathToInstance, pathToPackings,
  mutation = "inversion", mu = 1L, n.changes = n.runs,# max.evals = max.evals.initial,
  tau = tau, max.evals.initial = max.evals.initial, full = TRUE)


traj = lapply(res, function(r) r$trajectory)
traj = do.call(c, traj)
plot(traj, type = "s")
abline(v = c(max.evals.initial + (0:n.runs) * tau), col = scales::alpha("blue", 0.12), lwd = 2, lty = 3)
