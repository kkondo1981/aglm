context("test_aglm")
library(aglm)


test_that("Check the types and forms of return value of aglm() and predict.aglm().", {
  set.seed(12345)

  # size of observations
  nobs <- 1000

  # Randomly generates a numeric vector (as quantitative data)
  quan_var <- runif(nobs)

  # Randomly generates a character vector (as qualitative data with 5 levels)
  qual_var <- factor(paste0("level_", sample(1:5, nobs, replace=TRUE)))

  # Create the whole input data frame
  x <- data.frame(quan_var, qual_var)
  colnames(x) <- c("quan_var", "qual_var")

  # Generates non-linear reponse
  y <- sign(quan_var) * (abs(quan_var) ** 4) * (qual_var != "level_1")
  y <- y + 0.1 * rnorm(length(y))

  lambda.min <- cv.aglm(x, y, family="gaussian")@lambda.min
  res <- aglm(x, y, family="gaussian", lambda=lambda.min)

  expect_true("AccurateGLM" %in% class(res))
  expect_true("glmnet" %in% class(res@backend_models[[1]]))


  # Generates new predictive variables
  n_new_obs <- 100
  new_quan_var <- runif(n_new_obs)
  new_qual_var <- factor(paste0("level_", sample(1:5, n_new_obs, replace=TRUE)))
  newx <- data.frame(new_quan_var, new_qual_var)
  colnames(newx) <- c("quan_var", "qual_var")

  # Predict values of y for newx
  #y_true <- sign(new_quan_var) * (abs(new_quan_var) ** 4) * (new_qual_var != "level_1")
  y_pred <- predict(res, newx)
  #plot(new_quan_var, y_pred)
  #points(new_quan_var, y_true, col="red")
  expect_equal(class(y_pred), "matrix")
  expect_equal(length(y_pred), n_new_obs)
})

test_that("Check for predict.AGLM_CV().", {
  set.seed(12345)

  # size of observations
  nobs <- 1000

  # Randomly generates a numeric vector (as quantitative data)
  quan_var <- runif(nobs)

  # Randomly generates a character vector (as qualitative data with 5 levels)
  qual_var <- factor(paste0("level_", sample(1:5, nobs, replace=TRUE)))

  # Create the whole input data frame
  x <- data.frame(quan_var, qual_var)
  colnames(x) <- c("quan_var", "qual_var")

  # Generates non-linear reponse
  y <- sign(quan_var) * (abs(quan_var) ** 4) * (qual_var != "level_1")
  y <- y + 0.1 * rnorm(length(y))

  res <- cv.aglm(x, y, family="gaussian", keep=TRUE)

  expect_true("AccurateGLM" %in% class(res))
  expect_true("glmnet" %in% class(res@backend_models[[1]]))
  expect_true(class(res@fit.preval) == "matrix")
  expect_true(class(res@foldid) == "integer")

  # Generates new predictive variables
  n_new_obs <- 100
  new_quan_var <- runif(n_new_obs)
  new_qual_var <- factor(paste0("level_", sample(1:5, n_new_obs, replace=TRUE)))
  newx <- data.frame(new_quan_var, new_qual_var)
  colnames(newx) <- c("quan_var", "qual_var")

  # Predict values of y for newx
  #y_true <- sign(new_quan_var) * (abs(new_quan_var) ** 4) * (new_qual_var != "level_1")
  y_pred <- predict(res, newx, s=res@lambda.min)
  #plot(new_quan_var, y_pred)
  #points(new_quan_var, y_true, col="red")
  expect_equal(class(y_pred), "matrix")
  expect_equal(length(y_pred), n_new_obs)
})


test_that("Check for logical features", {
  set.seed(12345)

  # size of observations and variables
  nobs <- 1000
  nvars <- 2

  # a random generated logical variables
  x <- matrix(sample(c(TRUE, FALSE), nobs * nvars, replace=TRUE), nobs, nvars)

  # Generates non-linear reponse
  y <- xor(x[, 1], x[, 2])

  res <- cv.aglm(x, y, family="gaussian", keep=TRUE)

  expect_true("AccurateGLM" %in% class(res))
  expect_true("glmnet" %in% class(res@backend_models[[1]]))
  expect_true(class(res@fit.preval) == "matrix")
  expect_true(class(res@foldid) == "integer")

  # Generates new predictive variables
  n_new_obs <- 100

  newx <- matrix(sample(c(TRUE, FALSE), n_new_obs * nvars, replace=TRUE), n_new_obs, nvars)

  # Predict values of y for newx
  y_pred <- predict(res, newx, s=res@lambda.min)
  #y_true <- xor(newx[, 1], newx[, 2])
  #plot(y_true, y_pred)
  expect_equal(class(y_pred), "matrix")
  expect_equal(length(y_pred), n_new_obs)
})

test_that("Check for binomial family", {
  nobs <- 1000
  x1 <- rnorm(nobs); x2 <- rnorm(nobs); x <- cbind(x1, x2)
  y <- 1 * ((atan(0.25 * x1 - 0.5 * x2) / pi + 0.5) > 0.5)
  model <- aglm(x, y, family = "binomial", alpha = 1, lambda = 0.003)

  newx1 <- rnorm(100); newx2 <- rnorm(100); newx <- cbind(newx1, newx2)
  aglm.pred <- predict(model, newx)

  expect_equal(length(aglm.pred), 100)
})
