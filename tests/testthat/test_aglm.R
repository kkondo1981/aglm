context("test_aglm")
library(aglm)


test_that("Check the types and forms of return value of aglm() and predict.aglm().", {
  set.seed(12345)

  # Generates random data
  nobs <- 500
  n_quan_vars <- 20
  quan_var <- matrix(runif(nobs * n_quan_vars), nobs, n_quan_vars)
  qual_var <- factor(paste0("level_", sample(1:5, nobs, replace=TRUE)))
  x <- data.frame(quan_var, qual_var)
  colnames(x) <- c(lapply(1:n_quan_vars, function(i){sprintf("quan_var%d", i)}), "qual_var")
  y <- sign(quan_var[,1]) * (abs(quan_var[,2]) ** 4) * (qual_var != "level_1")
  y <- y + 0.1 * rnorm(length(y))

  # Generates new variables
  n_new_obs <- 100
  new_quan_var <- matrix(runif(n_new_obs * n_quan_vars), n_new_obs, n_quan_vars)
  new_qual_var <- factor(paste0("level_", sample(1:5, n_new_obs, replace=TRUE)))
  newx <- data.frame(new_quan_var, new_qual_var)
  colnames(newx) <- c(lapply(1:n_quan_vars, function(i){sprintf("quan_var%d", i)}), "qual_var")
  y_true <- sign(new_quan_var[,1]) * (abs(new_quan_var[,2]) ** 4) * (new_qual_var != "level_1")


  # cv.aglm
  cv_results <- cv.aglm(x, y, family="gaussian")
  lambda.min <- cv_results@lambda.min
  res <- aglm(x, y, family="gaussian", lambda=lambda.min)
  y_pred <- predict(res, newx)
  RMSE1 <- sqrt(mean((y_pred - y_true)^2))

  expect_true("AccurateGLM" %in% class(res))
  expect_true("glmnet" %in% class(res@backend_models[[1]]))
  expect_true("matrix" %in% class(y_pred))
  expect_equal(length(y_pred), n_new_obs)

  # cva.aglm
  cva_results <- cva.aglm(x, y, family="gaussian")
  alpha.min <- cva_results@alpha.min
  lambda.min <- cva_results@lambda.min
  res <- aglm(x, y, family="gaussian", alpha=alpha.min, lambda=lambda.min)
  y_pred <- predict(res, newx)
  RMSE2 <- sqrt(mean((y_pred - y_true)^2))

  # The result of cva.aglm() must be better than that of cv.aglm() for trainning data.
  expect_true(RMSE2 <= RMSE1)
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
  expect_true("matrix" %in% class(res@fit.preval))
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
  expect_true("matrix" %in% class(y_pred))
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
  expect_true("matrix" %in% class(res@fit.preval))
  expect_true(class(res@foldid) == "integer")

  # Generates new predictive variables
  n_new_obs <- 100

  newx <- matrix(sample(c(TRUE, FALSE), n_new_obs * nvars, replace=TRUE), n_new_obs, nvars)

  # Predict values of y for newx
  y_pred <- predict(res, newx, s=res@lambda.min)
  #y_true <- xor(newx[, 1], newx[, 2])
  #plot(y_true, y_pred)
  expect_true("matrix" %in% class(y_pred))
  expect_equal(length(y_pred), n_new_obs)
})

test_that("Check for binomial family", {
  set.seed(12345)

  nobs <- 1000
  x1 <- rnorm(nobs); x2 <- rnorm(nobs); x <- cbind(x1, x2)
  y <- 1 * ((atan(0.25 * x1 - 0.5 * x2) / pi + 0.5) > 0.5)
  model <- aglm(x, y, family = binomial(), alpha = 1, lambda = 0.003)

  newx1 <- rnorm(100); newx2 <- rnorm(100); newx <- cbind(newx1, newx2)
  aglm.pred <- predict(model, newx)

  expect_equal(length(aglm.pred), 100)
})


test_that("Check for poisson family", {
  set.seed(12345)

  nobs <- 100
  x <- runif(nobs) * 20
  y <- rpois(nobs, x)
  cv_result <- cv.aglm(x, y, family = "poisson", alpha = 1)

  newx <- runif(100) * 20
  y_pred <- predict(cv_result, newx, s=cv_result@lambda.min)

  expect_equal(length(y_pred), 100)
  expect_equal(names(cv_result@name), "deviance")
})
