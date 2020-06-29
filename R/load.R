#' @title TTP instance loader.
#'
#' @description Loads TTP instance.
#'
#' @param path [\code{character(1)}]\cr
#'   Path to TTP instance.
#' @param ... [any]\cr
#'   Possibility to overwrite atomic parameters, e.g. the knapsack capacity \code{capacity}
#'   or the maximum velocity \code{vmax}. This is useful if parameters are to
#'   be varied in studies. Note that no sanity checks are performed here. So you should
#'   know what you do.
#' @return [\code{ttp_instance}] Basically a list with class \dQuote{ttp_instance}.
#' @examples
#' \dontrun{
#' fp = "path/to/my/ttp/instance.ttp"
#' x = loadProblem(fp)
#' x = loadProblem(fp, vmax = 20, R = 20000)
#' }
#' @export
loadProblem = function(path, ...) {
  checkmate::assertFileExists(path, extension = "ttp", access = "r")
  overwrite.args = list(...)
  if (length(overwrite.args) > 0L)
    checkmate::assertSubset(names(overwrite.args), choices = c("capacity", "vmin", "vmax", "R"))

  meta = readLines(path, n = 9L)
  meta = lapply(meta, parseLine)
  names(meta) = c("name", "type", "n", "m", "capacity", "vmin", "vmax", "R", "edge_weight_type")
  meta$n = as.integer(meta$n)
  meta$m = as.integer(meta$m)
  meta$capacity = as.numeric(meta$capacity)
  meta$vmin = as.numeric(meta$vmin)
  meta$vmax = as.numeric(meta$vmax)
  meta$R = as.numeric(meta$R)

  if (length(overwrite.args) > 0L)
    meta = BBmisc::insert(meta, overwrite.args)

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

#' @title TTP instance writer.
#'
#' @description Writes TTP instance to file.
#'
#' @param x [\code{ttp_instance}]\cr
#'   TTP instance.
#' @param path [\code{character(1)}]\cr
#'   Path to file.
#' @param overwrite [\code{logical(1)}]\cr
#'   Should file be overwritten if it already exists?
#'   Default is \code{FALSE}.
#' @return [\code{invisible(TRUE)}]
#' @export
writeProblem = function(x, path, overwrite = FALSE, ...) {
  checkmate::assertClass(x, "ttp_instance")
  checkmate::assertFlag(overwrite)
  checkmate::assertPathForOutput(path, extension = "ttp", overwrite = TRUE)
  if (overwrite & file.exists(path))
    unlink(path)

  con = file(path, "w", encoding = "UTF-8")
  on.exit(close(con))

  header = c(
    sprintf("PROBLEM NAME: %s", x$name),
    sprintf("KNAPSACK DATA TYPE: %s", x$type),
    sprintf("DIMENSION: %i", x$n),
    sprintf("NUMBER OF ITEMS: %i", x$m),
    sprintf("CAPACITY OF KNAPSACK: %i", x$capacity),
    sprintf("MIN SPEED: %f", x$vmin),
    sprintf("MAX SPEED: %f", x$vmax),
    sprintf("RENTING RATIO: %f", x$R),
    sprintf("EDGE_WEIGHT_TYPE: %s", x$edge_weight_type),
    "NODE_COORD_SECTION	(INDEX, X, Y):")

  writeLines(header, con = con, sep = "\n")
  write.table(x$coordinates, file = con, quote = FALSE, row.names = TRUE, col.names = FALSE, fileEncoding = "UTF-8")
  writeLines("ITEMS SECTION	(INDEX, PROFIT, WEIGHT, ASSIGNED NODE NUMBER):", con = con, sep = "\n")
  write.table(x$items, file = con, quote = FALSE, row.names = TRUE, col.names = FALSE, fileEncoding = "UTF-8")
  return(invisible(TRUE))
}

# Helper function to parse a single line in TTP file header.
parseLine = function(line) {
  strsplit(line, split = "[[:space:]]*:[[:space:]]*")[[1L]][2L]
}

#' @export
print.ttp_instance = function(x, ...) {
  catf("TTP:                  %s (%s)\n", x$name, x$type)
  catf("#Nodes:               %i\n", x$n)
  catf("#Items:               %i (%i per node)\n", x$m, x$m / (x$n - 1))
  catf("Knapsack capacity:    %.2f\n", x$capacity)
  catf("Min. velocity (vmin): %.2f\n", x$vmin)
  catf("Max. velocity (vmax): %.2f\n", x$vmax)
  catf("Renting price:        %.2f\n", x$R)
  catf("First 10 items:\n")
  print(head(x$items, n = 10L))
}
