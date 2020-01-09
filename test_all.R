library(methods)
library(devtools)
library(testthat)

if (interactive()) {
  load_all(".")
} else {
  library(TTP)
}

test_dir("tests/testthat")
