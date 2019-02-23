context("binning")
library(aglm)

EPS <- 1e-10

test_that("createEqualWidthBins()'s outputs are correct.", {
  expect_equal(createEqualWidthBins(0, 1, 3), c(0.0, 0.5, 1.0), tolerance=EPS)
  expect_equal(createEqualWidthBins(-33, 22, 14), (0:13) * ((22 + 33) / 13) - 33, tolerance=EPS)
  expect_equal(length(createEqualWidthBins(-323434, 134034193, 1234568)), 1234568)
})

test_that("createEqualFreqBins()'s outputs are correct.", {
  expect_equal(createEqualFreqBins(c(0.1, 0.3, 0.5, 0.8), 4), c(0.1, 0.3, 0.5, 0.8), tolerance=EPS)
  expect_equal(createEqualFreqBins(c(0.1, 0.3, 0.5, 0.8), 3), c(0.1, 0.4, 0.8), tolerance=EPS)
  expect_equal(length(createEqualFreqBins(rnorm(1000), 100)), 100, tolerance=EPS)

  x <- ordered(c(1, 1, 1, 1, 1, 2, 2, 3, 3, 4))
  expect_equal(executeBinning(x, nbin.max=100)$labels, as.integer(x) + 1)
  expect_equal(executeBinning(x, nbin.max=4)$labels, c(2, 2, 2, 2, 2, 3, 3, 3, 3, 4))
})

test_that("executeBinning()'s outputs are correct.", {
  expect_equal(executeBinning(c(-1, 0, 1.5, 3), breaks=createEqualWidthBins(0, 3, 3))$labels, 1:4)
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3)$labels, 2:4)
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3)$breaks, c(0, 1, 3))
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3, method="freq")$labels, c(2, 3, 4))
  expect_equal(executeBinning(c(0, 1, 3), nbin.max=3, method="freq")$breaks, c(0, 1, 3))
  expect_equal(executeBinning(c(0, 1.5, 3), nbin.max=4, method="width")$labels, c(2, 3, 5))
  expect_equal(executeBinning(c(0, 1.5, 3), nbin.max=4, method="width")$breaks, 0:3)
  expect_equal(executeBinning(c(0, 1, 3))$breaks, c(0, 1, 3))
  expect_equal(length(executeBinning(1:1000)$breaks), 100)
})
