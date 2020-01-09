library(ggplot2)
library(tidyverse)
library(scales)

devtools::load_all()

# load packings and run
pathToPackings = "packings/BenchmarkTTP_n100_p0.5/BenchmarkTTP_n100_p0.5_c10_1.txt"
pathToInstance = "instances/eil101-ttp/eil101_n1000_uncorr_01.ttp"
max.evals.initial = 10000L
tau = 1000L
n.runs = 30L
res = runTTPSolverInDynamicSetting(
  pathToInstance, pathToPackings,
  mutation = 1L, mu = 1L, n.changes = n.runs,# max.evals = max.evals.initial,
  tau = tau, max.evals.initial = max.evals.initial, full = TRUE)


traj = lapply(res, function(r) r$trajectory)
traj = do.call(c, traj)
plot(traj, type = "s")
abline(v = c(max.evals.initial + (0:n.runs) * tau), col = scales::alpha("blue", 0.12), lwd = 2, lty = 3)
#res = runTTPSolver(instance, packing = packing, initial.tour = initial.tour, max.iter = 10000)
#print(res)
