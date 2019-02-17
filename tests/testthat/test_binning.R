context("binning")
library(aglm)

EPS <- 1e-10

test_that("createEqualWidthBins()'s outputs are correct.", {
  expect_equal(createEqualWidthBins(0, 1, 2), c(0.0, 0.5, 1.0), tolerance=EPS)
  expect_equal(createEqualWidthBins(-33, 22, 13), (0:13) * ((22 + 33) / 13) - 33, tolerance=EPS)
  expect_equal(length(createEqualWidthBins(-323434, 134034193, 1234567)), 1234567 + 1)
})

test_that("createEqualFreqBins()'s outputs are correct.", {
  expect_equal(createEqualFreqBins(c(0.1, 0.3, 0.5, 0.8), 3), c(0.1, 0.3, 0.5, 0.8), tolerance=EPS)
  expect_equal(createEqualFreqBins(c(0.1, 0.3, 0.5, 0.8), 2), c(0.1, 0.4, 0.8), tolerance=EPS)
  expect_equal(length(createEqualFreqBins(rnorm(1000), 100)), 101, tolerance=EPS)

  x <- ordered(c(1, 1, 1, 1, 1, 2, 2, 3, 3, 4))
  expect_equal(executeBinning(x, nbin.max=100)$labels, as.integer(x))
  expect_equal(executeBinning(x, nbin.max=2)$labels, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2))
})

test_that("executeBinning()'s outputs are correct.", {
  expect_equal(executeBinning(c(0, 1.5, 3), breaks=createEqualWidthBins(0, 3, 3))$labels, 1:3)
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3)$labels, 1:3)
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3)$breaks, c(-Inf, 2/3, 5/3, Inf))
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3, method="freq")$labels, 1:3)
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3, method="freq")$breaks, c(-Inf, 2/3, 5/3, Inf))
  expect_equal(executeBinning(c(0, 1.5, 3), nbin.max=3, method="width")$labels, 1:3)
  expect_equal(executeBinning(c(0, 1.5, 3), nbin.max=3, method="width")$breaks, c(-Inf, 1, 2, Inf))
  expect_equal(executeBinning(c(0, 1, 3))$breaks, c(-Inf, 2/3, 5/3, Inf))
  expect_equal(length(executeBinning(1:1000)$breaks), 101)
  expect_equal(executeBinning(0, 1:3, allow_na=TRUE)$breaks, 1:3)
  expect_equal(executeBinning(0, 1:3, allow_na=FALSE)$breaks, c(-Inf, 2, Inf))
})
