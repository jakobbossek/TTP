#' @title TTP instance loader.
#'
#' @description Loads TTP instance.
#'
#' @param path [\code{character(1)}]\cr
#'   Path to TTP instance.
#' @return [\code{ttp_instance}] Basically a list with class \dQuote{ttp_instance}.
#' @export
load = function(path) {
  checkmate::assertFileExists(path, extension = "ttp", access = "r")
  meta = readLines(path, n = 9L)
  meta = lapply(meta, parseLine)
  names(meta) = c("name", "type", "n", "m", "capacity", "vmin", "vmax", "R", "edge_weight_type")
  meta$n = as.integer(meta$n)
  meta$m = as.integer(meta$m)
  meta$capacity = as.numeric(meta$capacity)
  meta$vmin = as.numeric(meta$vmin)
  meta$vmax = as.numeric(meta$vmax)
  meta$R = as.numeric(meta$R)

  n.skip = 10L
  meta$coordinates = read.table(path, header = FALSE, skip = n.skip, nrows = meta$n)[, 2:3, drop = FALSE]
  meta$distance.matrix = as.matrix(stats::dist(meta$coordinates, method = "euclidean"))

  n.skip = n.skip + meta$n + 1L
  items = read.table(path, header = FALSE, skip = n.skip, nrows = meta$m)[, 2:4, drop = FALSE]
  colnames(items) = c("profit", "weight", "nodenr")
  # IMPORTANT: sort items by assigned node number in ascending order.
  # This way we can efficiently add up weights and profits that belong to the same node
  #items = items[order(items$nodenr, decreasing = FALSE), ]
  meta$items = items

  class(meta) = c("ttp_instance", "tsp_instance")
  return(meta)
}

# Helper function to parse a single line in TTP file header.
parseLine = function(line) {
  strsplit(line, split = "[[:space:]]*:[[:space:]]*")[[1L]][2L]
}

#' @export
print.ttp_instance = function(x, ...) {
  catf("TTP:                  %s (%s)", x$name, x$type)
  catf("#Nodes:               %i", x$n)
  catf("#Items:               %i (%i per node)", x$m, x$m / (x$n - 1))
  catf("Knapsack capacity:    %.2f", x$capacity)
  catf("Min. velocity (vmin): %.2f", x$vmin)
  catf("Max. velocity (vmax): %.2f", x$vmax)
  catf("Renting price:        %.2f", x$R)
  catf("First 10 items:")
  print(head(x$items, n = 10L))
}
