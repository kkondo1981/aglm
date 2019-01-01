context("binning")
library(aglm)

EPS <- 1e-10

test_that("createEvenBins()'s outputs are correct.", {
  expect_equal(createEvenBins(0, 1, 2), c(0.0, 0.5, 1.0), tolerance=EPS)
  expect_equal(createEvenBins(-33, 22, 13), (0:13) * ((22 + 33) / 13) - 33, tolerance=EPS)
  expect_equal(length(createEvenBins(-323434, 134034193, 1234567)), 1234567 + 1)
})

test_that("executeBinning()'s outputs are correct.", {
  expect_equal(executeBinning(c(0, 1.5, 3), breaks=createEvenBins(0, 3, 3))$labels, 1:3)
  expect_equal(executeBinning(c(0, 1.5, 3), nbin=3)$labels, 1:3)
  expect_equal(executeBinning(0, 1:3, allow_na=TRUE)$breaks, 1:3)
  expect_equal(executeBinning(0, 1:3, allow_na=FALSE)$breaks, c(-Inf, 2, Inf))
})
