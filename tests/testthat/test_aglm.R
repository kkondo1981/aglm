context("aglm")
library(aglm)

test_that("Check the types and forms of return value of aglm() when x_UD is specified.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3
  nvar_UD <- 1

  x <- matrix(rnorm(nobs * (nvar - nvar_UD)), nobs, nvar - nvar_UD)
  x_UD <- matrix(sample(1:10, nobs * nvar_UD, replace=TRUE))
  y <- rnorm(nobs)

  res <- aglm(x, y, x_UD=x_UD, family="gaussian")

  expect_true("aglm" %in% class(res))
  expect_true("glmnet" %in% class(res$backend_result))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$name, "X_1")
  expect_equal(res$vars_info[[1]]$type, "O")
  expect_equal(res$vars_info[[nvar]]$name, "X_UD_1")
  expect_equal(res$vars_info[[nvar]]$type, "U")
  expect_equal(res$vars_info[[nvar]]$levels, paste0("", 1:10))
})


test_that("Check the types and forms of return value of aglm() when UD_vars is given as integer.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3
  nvar_UD <- 1

  x <- matrix(rnorm(nobs * (nvar - nvar_UD)), nobs, nvar - nvar_UD)
  x_UD <- matrix(sample(1:10, nobs * nvar_UD, replace=TRUE), nobs, nvar_UD)
  y <- rnorm(nobs)

  z <- data.frame(x, x_UD)
  names(z) <- NULL
  res <- aglm(z, y, UD_vars=3, family="gaussian")

  expect_true("aglm" %in% class(res))
  expect_true("glmnet" %in% class(res$backend_result))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$name, "X_1")
  expect_equal(res$vars_info[[1]]$type, "O")
  expect_equal(res$vars_info[[nvar]]$name, "X_UD_1")
  expect_equal(res$vars_info[[nvar]]$type, "U")
  expect_equal(res$vars_info[[nvar]]$levels, paste0("", 1:10))
})


test_that("Check the types and forms of return value of aglm() when UD_vars is given as character.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3
  nvar_UD <- 1

  x <- matrix(rnorm(nobs * (nvar - nvar_UD)), nobs, nvar - nvar_UD)
  x_UD <- matrix(sample(1:10, nobs * nvar_UD, replace=TRUE), nobs, nvar_UD)
  y <- rnorm(nobs)

  z <- data.frame(x, x_UD)
  names(z) <- c("A", "B", "C")
  res <- aglm(z, y, UD_vars="C", family="gaussian")

  expect_true("aglm" %in% class(res))
  expect_true("glmnet" %in% class(res$backend_result))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$name, "A")
  expect_equal(res$vars_info[[1]]$type, "O")
  expect_equal(res$vars_info[[nvar]]$name, "C")
  expect_equal(res$vars_info[[nvar]]$type, "U")
  expect_equal(res$vars_info[[nvar]]$levels, paste0("", 1:10))
})


test_that("Check the types and forms of return value of aglm() when x_UD and UD_vars are both NULL.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3
  nvar_UD <- 1

  x <- matrix(rnorm(nobs * (nvar - nvar_UD)), nobs, nvar - nvar_UD)
  x_UD <- matrix(as.character(sample(1:10, nobs * nvar_UD, replace=TRUE)), nobs, nvar_UD)
  y <- rnorm(nobs)

  z <- data.frame(x, x_UD)
  names(z) <- c("A", "B", "C")
  res <- aglm(z, y, family="gaussian")

  expect_true("aglm" %in% class(res))
  expect_true("glmnet" %in% class(res$backend_result))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$name, "A")
  expect_equal(res$vars_info[[1]]$type, "O")
  expect_equal(res$vars_info[[nvar]]$name, "C")
  expect_equal(res$vars_info[[nvar]]$type, "U")
  expect_equal(res$vars_info[[nvar]]$levels, sort(paste0("", 1:10)))
})


# test_that("Check if aglm()'s inferences are correct"), {
# Currently not implemented
# })
