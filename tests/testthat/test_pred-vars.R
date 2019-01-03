context("PredVars")
library(aglm)


test_that("Check the creation of PredVars objects when both x and x_UD are specified.", {
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

  res <- newPredVars(x=x, x_UD=x_UD, append_interaction_vars=FALSE)
  expect_true("PredVars" %in% class(res))
  expect_equal(dim(res@data), c(nobs, nvar))
  expect_equal(length(res@vars_info), nvar)
  expect_equal(res@vars_info[[1]]$idx, 1)
  expect_equal(res@vars_info[[1]]$name, var_names[1])
  expect_equal(res@vars_info[[1]]$data_column, 1)
  expect_equal(res@vars_info[[1]]$type, "O")
  expect_equal(res@vars_info[[3]]$idx, nvar)
  expect_equal(res@vars_info[[3]]$name, var_names[3])
  expect_equal(res@vars_info[[3]]$data_column, 3)
  expect_equal(res@vars_info[[3]]$type, "U")
  expect_equal(res@vars_info[[3]]$dummy_info$levels, sort(paste0("", 1:10)))
})


test_that("Check the creation of PredVars objects when x and UD_vars are specified.", {
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

  res <- newPredVars(x=data.frame(x, x_UD), UD_vars=3, append_interaction_vars=FALSE)
  expect_true("PredVars" %in% class(res))
  expect_equal(dim(res@data), c(nobs, nvar))
  expect_equal(length(res@vars_info), nvar)
  expect_equal(res@vars_info[[1]]$idx, 1)
  expect_equal(res@vars_info[[1]]$name, var_names[1])
  expect_equal(res@vars_info[[1]]$data_column, 1)
  expect_equal(res@vars_info[[1]]$type, "O")
  expect_equal(res@vars_info[[3]]$idx, nvar)
  expect_equal(res@vars_info[[3]]$name, var_names[3], "_UD")
  expect_equal(res@vars_info[[3]]$data_column, 3)
  expect_equal(res@vars_info[[3]]$type, "U")
  expect_equal(res@vars_info[[3]]$dummy_info$levels, sort(paste0("", 1:10)))

  res <- newPredVars(x=data.frame(x, x_UD), UD_vars="Var_3", append_interaction_vars=FALSE)
  expect_true("PredVars" %in% class(res))
  expect_equal(dim(res@data), c(nobs, nvar))
  expect_equal(length(res@vars_info), nvar)
  expect_equal(res@vars_info[[1]]$idx, 1)
  expect_equal(res@vars_info[[1]]$name, var_names[1])
  expect_equal(res@vars_info[[1]]$data_column, 1)
  expect_equal(res@vars_info[[1]]$type, "O")
  expect_equal(res@vars_info[[3]]$idx, nvar)
  expect_equal(res@vars_info[[3]]$name, var_names[3])
  expect_equal(res@vars_info[[3]]$data_column, 3)
  expect_equal(res@vars_info[[3]]$type, "U")
  expect_equal(res@vars_info[[3]]$dummy_info$levels, sort(paste0("", 1:10)))
})


test_that("Check the creation of PredVars objects when only x_UD is specified.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 3

  var_names <- paste0("Var_", 1:nvar)

  x_UD <- matrix(paste0("", sample(1:10, nobs * nvar, replace=TRUE)), nobs, nvar)
  colnames(x_UD) <- var_names

  res <- newPredVars(x_UD=x_UD, append_interaction_vars=FALSE)

  expect_true("PredVars" %in% class(res))
  expect_equal(dim(res@data), c(nobs, nvar))
  expect_equal(length(res@vars_info), nvar)
  expect_equal(res@vars_info[[1]]$idx, 1)
  expect_equal(res@vars_info[[1]]$name, var_names[1])
  expect_equal(res@vars_info[[1]]$data_column, 1)
  expect_equal(res@vars_info[[1]]$type, "U")
  expect_equal(res@vars_info[[1]]$dummy_info$levels, sort(paste0("", 1:10)))
})


test_that("Check the creation of PredVars objects when only x is specified.", {
  set.seed(12345)
  nobs <- 100
  nvar <- 2

  var_names <- paste0("Var_", 1:nvar)

  x <- matrix(rnorm(nobs * nvar), nobs, nvar)
  colnames(x) <- var_names

  res <- newPredVars(x=x, append_interaction_vars=FALSE)

  expect_true("PredVars" %in% class(res))
  expect_equal(dim(res@data), c(nobs, nvar))
  expect_equal(length(res@vars_info), nvar)
  expect_equal(res@vars_info[[1]]$idx, 1)
  expect_equal(res@vars_info[[1]]$name, var_names[1])
  expect_equal(res@vars_info[[1]]$data_column, 1)
  expect_equal(res@vars_info[[1]]$type, "O")
})


test_that("Check getDesignMatrix().", {
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

  preds <- newPredVars(x=x, x_UD=x_UD, append_interaction_vars=FALSE)

  dm <- getDesignMatrix(preds, standardize_quantitative_vars=TRUE)
  expect_equal(dim(dm), c(nobs, nvar_OD * (1 + 20) + nvar_UD * 9))
  expect_equal(sort(unique(as.numeric(dm[, 2:21]))), c(0, 1))
  expect_equal(sort(unique(as.numeric(dm[, 23:42]))), c(0, 1))
  expect_equal(sort(unique(as.numeric(dm[, 43:51]))), c(0, 1))
  expect_equal(colnames(dm)[1:3], c("Var_1", "Var_1_dummy_1", "Var_1_dummy_2"))
  expect_equal(mean(dm[, 1]), 0)
  expect_equal(sd(dm[, 1]), 1)

  dm <- getDesignMatrix(preds, standardize_quantitative_vars=FALSE)
  expect_equal(mean(dm[, 1]), mean(x[, 1]))
  expect_equal(sd(dm[, 1]), sd(x[, 1]))


  preds <- newPredVars(x=x, x_UD=x_UD, append_interaction_vars=TRUE)
  dm <- getDesignMatrix(preds)
  expect_equal(dim(dm), c(nobs, nvar_OD * (1 + 20) + nvar_UD * 9 + nvar_OD * (nvar_OD - 1) / 2 + nvar_OD * nvar_UD * 9 + nvar_UD * (nvar_UD - 1) / 2 * 9 * 9))
  expect_equal(dm[, "Var_1_x_Var_2"], dm[, "Var_1"] * dm[, "Var_2"])
  expect_equal(dm[, "Var_1_x_Var_3_1_1"], dm[, "Var_1"] * dm[, "Var_3_dummy_1"])


  # test for creations of practical size design matrixnobs <- 100
  nobs <- 100
  nvar_OD <- 20
  nvar_UD <- 20
  nvar <- nvar_OD + nvar_UD

  x <- matrix(rnorm(nobs * nvar_OD), nobs, nvar_OD)
  colnames(x) <- var_names[1:nvar_OD]

  x_UD <- matrix(paste0("", sample(1:10, nobs * nvar_UD, replace=TRUE)), nobs, nvar_UD)
  colnames(x_UD) <- var_names[-(1:nvar_OD)]

  preds <- newPredVars(x=x, x_UD=x_UD, append_interaction_vars=TRUE)
  dm <- getDesignMatrix(preds)
  expect_equal(dim(dm), c(nobs, nvar_OD * (1 + 20) + nvar_UD * 9 + nvar_OD * (nvar_OD - 1) / 2 + nvar_OD * nvar_UD * 9 + nvar_UD * (nvar_UD - 1) / 2 * 9 * 9))
})
