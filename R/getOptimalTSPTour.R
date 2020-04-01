#' @title Get optimal TSP tour from file.
#'
#' @description Given a problem name, e.g. \dQuote{a280} and a path to a
#' directory with \dQuote{.opt.tour} files in tsplib tour format the function
#' reads the file and returns the permutation.
#'
#' @param prob.name [\code{string}]\cr
#'   Problem name.
#' @param path.to.tsplib [\code{filepath}]\cr
#'   Absolute/relative path to tsplib instances and their tour files.
#' @return [\code{integer}] Permutation.
#' @export
getOptimalTSPTour = function(prob.name, path.to.tsplib) {
  checkmate::assertString(prob.name)
  checkmate::assertDirectory(path.to.tsplib, access = "r")
  f = sprintf("%s.opt.tour", prob.name)
  f = file.path(path.to.tsplib, f)
  if (!file.exists(f)) {
    BBmisc::stopf("[getTSPTour] There is no TSP tour file for problem '%s'.", prob.name)
  }
  salesperson::readTSPlibTOURFile(f)$tour
}
