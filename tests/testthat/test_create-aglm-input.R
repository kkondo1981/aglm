context("create-aglm-input")
library(aglm)


test_that("Check the return value of createAglmInput() when both x and x_UD are specified.", {
  set.seed(12345)
  nobs <- 100
  nvar_OD <- 2
  nvar_UD <- 1
  nvar <- nvar_OD + nvar_UD

  var_names <- paste0("Var_", 1:nvar)

  x <- matrix(rnorm(nobs * nvar_OD), nobs, nvar_OD)
  colnames(x) <- var_names[1:nvar_OD]

  x_UD <- matrix(paste0("", sample(1:10, nobs * nvar_UD, replace=TRUE)))
  colnames(x_UD) <- var_names[-(1:nvar_OD)]

  res <- createAglmInput(x=x, x_UD=x_UD)

  expect_true("aglm.input" %in% class(res))
  expect_equal(dim(res$x_all), c(nobs, nvar))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$idx, 1)
  expect_equal(res$vars_info[[1]]$name, var_names[1])
  expect_equal(res$vars_info[[1]]$type, "O")
  expect_equal(res$vars_info[[nvar]]$idx, nvar)
  expect_equal(res$vars_info[[nvar]]$name, var_names[nvar])
  expect_equal(res$vars_info[[nvar]]$type, "U")
  expect_equal(res$vars_info[[nvar]]$dummy_info$levels, sort(paste0("", 1:10)))
})


test_that("Check the return value of createAglmInput() when x and UD_vars are specified.", {
  set.seed(12345)
  nobs <- 100
  nvar_OD <- 2
  nvar_UD <- 1
  nvar <- nvar_OD + nvar_UD

  var_names <- paste0("Var_", 1:nvar)

  x <- matrix(rnorm(nobs * nvar_OD), nobs, nvar_OD)
  colnames(x) <- var_names[1:nvar_OD]

  x_UD <- matrix(paste0("", sample(1:10, nobs * nvar_UD, replace=TRUE)), nobs, nvar_UD)
  colnames(x_UD) <- var_names[-(1:nvar_OD)]

  res <- createAglmInput(x=data.frame(x, x_UD), UD_vars=3)

  expect_true("aglm.input" %in% class(res))
  expect_equal(dim(res$x_all), c(nobs, nvar))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$idx, 1)
  expect_equal(res$vars_info[[1]]$name, var_names[1])
  expect_equal(res$vars_info[[1]]$type, "O")
  expect_equal(res$vars_info[[nvar]]$idx, nvar)
  expect_equal(res$vars_info[[nvar]]$name, var_names[nvar])
  expect_equal(res$vars_info[[nvar]]$type, "U")
  expect_equal(res$vars_info[[nvar]]$dummy_info$levels, sort(paste0("", 1:10)))

  res <- createAglmInput(x=data.frame(x, x_UD), UD_vars="Var_3")
  expect_true("aglm.input" %in% class(res))
  expect_equal(dim(res$x_all), c(nobs, nvar))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$idx, 1)
  expect_equal(res$vars_info[[1]]$name, var_names[1])
  expect_equal(res$vars_info[[1]]$type, "O")
  expect_equal(res$vars_info[[nvar]]$idx, nvar)
  expect_equal(res$vars_info[[nvar]]$name, var_names[nvar])
  expect_equal(res$vars_info[[nvar]]$type, "U")
  expect_equal(res$vars_info[[nvar]]$dummy_info$levels, sort(paste0("", 1:10)))
})


test_that("Check the return value of createAglmInput() when only x_UD is specified.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3

  var_names <- paste0("Var_", 1:nvar)

  x_UD <- matrix(paste0("", sample(1:10, nobs * nvar, replace=TRUE)), nobs, nvar)
  colnames(x_UD) <- var_names

  res <- createAglmInput(x_UD=x_UD)

  expect_true("aglm.input" %in% class(res))
  expect_equal(dim(res$x_all), c(nobs, nvar))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$idx, 1)
  expect_equal(res$vars_info[[1]]$name, var_names[1])
  expect_equal(res$vars_info[[1]]$type, "U")
  expect_equal(res$vars_info[[1]]$dummy_info$levels, sort(paste0("", 1:10)))
})


test_that("Check the return value of createAglmInput() when only x is specified.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 2

  var_names <- paste0("Var_", 1:nvar)

  x <- matrix(rnorm(nobs * nvar), nobs, nvar)
  colnames(x) <- var_names

  res <- createAglmInput(x=x)

  expect_true("aglm.input" %in% class(res))
  expect_equal(dim(res$x_all), c(nobs, nvar))
  expect_equal(length(res$vars_info), nvar)
  expect_equal(res$vars_info[[1]]$idx, 1)
  expect_equal(res$vars_info[[1]]$name, var_names[1])
  expect_equal(res$vars_info[[1]]$type, "O")
})

