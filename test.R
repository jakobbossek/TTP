library(ggplot2)
library(tidyverse)
library(scales)

devtools::load_all()

# load packings and run
#set.seed(1)
pathToPackings = "packings/BenchmarkTTP_n500_p0.5/BenchmarkTTP_n500_p0.5_c50_1.txt"
#pathToInstance = "instances/a280-ttp/a280_n1395_bounded-strongly-corr_01.ttp"

pathToInstance = "instances/eil101-ttp/eil101_n500_uncorr_01.ttp"
packing = as.matrix(read.table(pathToPackings, sep = " ", header = FALSE))[1, ]
#packing = rep(1L, length(packing)) # equal results if all items are packed!
prob = load(pathToInstance)
print(prob)

init.tour = matrix(1:prob$n, nrow = 1L)

st = system.time({
res = runWTSPSolverR(
  pathToInstance, packing = packing, tours = init.tour,
  mutation = "inversion", mu = 1L, max.evals = 30000L, objective.type = "wtsp", extended.trajectory = TRUE)
})
print(st)

print(res$tour.length)
print(wtsp(res$tour, prob = prob, packing = packing))
print(ttp(res$tour, prob = prob, packing = packing))

print(res)

g = plotTrajectories(res$trajectory) + ggplot2::facet_wrap(.~Objective, scales = "free_y", nrow = 1L)
print(g)
ggsave("trajectory_example.pdf", plot = g, width = 15, height = 3, device = cairo_pdf)
stop()



max.evals.initial = 10000L
tau = 25000L
n.changes = 5L

# st = system.time({
# res = runWTSPSolverR(
#   pathToInstance, as.matrix(read.table(pathToPackings, sep = " ", header = FALSE))[1, ],
#   mutation = "inversion", mu = 20L, max.evals = 1000000)
# })
# print(st)
# stop()

res = runWTSPSolverInDynamicSetting(
  pathToInstance, pathToPackings,
  mutation = "inversion", start.from.scratch = FALSE, mu = 20L,
  survival.strategy = "classic",
  objective.type = "wtsp",
  n.changes = n.changes,# max.evals = max.evals.initial,
  tau = tau, max.evals.initial = max.evals.initial, full = TRUE)


traj = lapply(res, function(r) r$trajectory)
traj = do.call(c, traj)
plot(traj, type = "s")#, xlim = c(floor(0.9 * max.evals.initial), max.evals.initial + tau * n.changes))
#plot(log(traj), type = "s", ylim = c(18, 18.3))#, xlim = c(floor(0.9 * max.evals.initial), max.evals.initial + tau * n.changes))

abline(v = c(max.evals.initial + (0:n.changes) * tau), col = scales::alpha("blue", 0.12), lwd = 2, lty = 3)
