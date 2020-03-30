#' @title Load packing(s)
#'
#' @description Given a path to a file with packings, i.e. rowwise strings
#' from \eqn{\{0, 1\}} the function imports the packings and returns a matrix.
#'
#' @param path [\code{string}]\cr
#'   File path to packings.
#' @param which [\code{integer}]\cr
#'   Which packings should be returned?
#'   Default is \code{NULL} which means all packings.
#' @return
#' @export
loadPackings = function(path, which = NULL) {
  checkmate::assertFileExists(path, access = "r")
  packings = as.matrix(read.table(path, header = FALSE, sep = " "))
  if (is.null(which))
    return(packings)

  nr = nrow(packings)
  which = checkmate::asInt(which, lower = 1L, upper = nr)
  return(packings[nr, , drop = TRUE])
}
