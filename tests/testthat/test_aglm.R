context("aglm")
library(aglm)

test_that("Check the types and forms of return value of aglm() in case where x is a PredVars object.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3
  nvar_UD <- 1

  x <- matrix(rnorm(nobs * (nvar - nvar_UD)), nobs, nvar - nvar_UD)
  x_UD <- matrix(sample(1:10, nobs * nvar_UD, replace=TRUE))
  y <- rnorm(nobs)

  res <- aglm(newPredVars(x=x, x_UD=x_UD), y, family="gaussian")

  expect_true("AccurateGLM" %in% class(res))
  expect_true("glmnet" %in% class(res@backend_models$glmnet))
  expect_equal(length(res@vars_info), nvar)
  expect_equal(res@vars_info[[1]]$name, "X1")
  expect_equal(res@vars_info[[1]]$type, "O")
  expect_equal(res@vars_info[[3]]$name, "x_UD")
  expect_equal(res@vars_info[[3]]$type, "U")
  expect_equal(res@vars_info[[3]]$dummy_info$levels, sort(paste0("", 1:10)))
})


test_that("Check the types and forms of return value of aglm() in case where x is not a PredVars object.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3
  nvar_UD <- 1

  x <- matrix(rnorm(nobs * (nvar - nvar_UD)), nobs, nvar - nvar_UD)
  x_UD <- matrix(sample(1:10, nobs * nvar_UD, replace=TRUE))
  y <- rnorm(nobs)

  res <- aglm(x=x, x_UD=x_UD, y=y, family="gaussian")

  expect_true("AccurateGLM" %in% class(res))
  expect_true("glmnet" %in% class(res@backend_models$glmnet))
  expect_equal(length(res@vars_info), nvar)
  expect_equal(res@vars_info[[1]]$name, "X1")
  expect_equal(res@vars_info[[1]]$type, "O")
  expect_equal(res@vars_info[[3]]$name, "x_UD")
  expect_equal(res@vars_info[[3]]$type, "U")
  expect_equal(res@vars_info[[3]]$dummy_info$levels, sort(paste0("", 1:10)))
})
