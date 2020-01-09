precalculatePackings = function(pathToInstance, p, times) {
  checkmate::assertFileExists(pathToInstance)
  p = checkmate::assertNumber(p, lower = 0, upper = 1)
  times = checkmate::asInt(times, lower = 2)

  # qucik and dirty
  itemline = readLines(pathToInstance, 4)[4L]
  k = as.integer(strsplit(itemline, split = "[[:space:]]*:[[:space:]]*")[[1]][2])

  packings = vector(mode = "list", length = times)
  packings[[1]] = sample(c(0, 1), size = k, replace = TRUE, prob = c(1 - p, p))
  for (i in 2:times) {
    packings[[i]] = sample(c(0, 1), size = k, replace = TRUE, prob = c(1 - p, p))
  }

  do.call(rbind, packings)
}
