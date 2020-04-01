context("Similarity measures for permutations")

test_that("measureDisorder", {
  p1 = 1:10
  n = length(p1)
  expect_equal(measureDisorder(p1, p1, method = "uncommonedges"), 0)
  expect_equal(measureDisorder(p1, rev(p1), method = "uncommonedges"), 0)
  expect_true(measureDisorder(p1, sample(p1), method = "uncommonedges", normalize = TRUE) %in% seq(0.0, 1.0, by = 0.1))

  expect_equal(measureDisorder(p1, p1, method = "inversion"), 0)
  expect_equal(measureDisorder(p1, rev(p1), method = "inversion"), n * (n - 1) / 2)
  expect_true(measureDisorder(p1, sample(p1), method = "inversion") <= (n * (n - 1) / 2))

  expect_equal(measureDisorder(p1, p1, method = "maxdist"), 0)
  expect_equal(measureDisorder(p1, rev(p1), method = "maxdist"), n - 1)
})
