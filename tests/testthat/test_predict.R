context("predict")
library(aglm)

createXY <- function(nobs, ncol_quan, ncol_qual) {
  set.seed(12345)

  # Randomly generate a quantitative explanatory variables
  quan_vars <- matrix(rnorm(nobs * ncol_quan), ncol=ncol_quan)
  colnames(quan_vars) <- paste0("quan_", 1:ncol_quan)

  # Randomly generate qalitative a explanatory variables with 4 levels
  qual_base <- sample(1:4, nobs * ncol_qual, replace=TRUE)
  qual_vars <- matrix(paste0("level_", qual_base), ncol=ncol_qual)
  colnames(qual_vars) <- paste0("qual_", 1:ncol_qual)

  # Nonlinear response values
  beta_quan <- matrix(1 * (runif(ncol_quan) > 0.7), nrow=ncol_quan)
  link <- quan_vars %*% beta_quan
  y <- sign(link) * sqrt(abs(link))# * (qual_vars[, 1] != "level_1")
  y <- y + 0.01 * rnorm(length(y))

  return(list(x=quan_vars, x_UD=qual_vars, y=y))
}

calcRMSE <- function(xy, train_idx, test_idx) {
  x <- xy$x
  x_UD <- xy$x_UD
  y <- xy$y

  preds_train <- newPredVars(x=x[train_idx, ], x_UD=x_UD[train_idx, ])
  y_train <- y[train_idx]
  fitted <- aglm(x=preds_train, y=y_train, lambda=1e-5)

  preds_test <- newPredVars(x=x[test_idx, ], x_UD=x_UD[test_idx, ])
  y_pred <- predict(fitted, newx=preds_test)
  y_test <- y[test_idx]
  rmse <- mean((y_pred - y_test) ** 2)

  return(rmse)
}


test_that("Check the types of return value.",{
  xy <- createXY(100, 2, 2)
  expect_equal(class(xy$y), "matrix")
  expect_equal(dim(xy$y), c(100, 1))
})


test_that("Check whether fitting error is improve when sample size grows.",{
  xy <- createXY(4 ** 5, 3, 3)
  rmse <- NULL
  for (nobs in 4 ** (1:5))
    rmse <- c(rmse, calcRMSE(xy, 1:nobs, 1:(4 ** 5)))
  expect_equal(sort(rmse, decreasing=TRUE), rmse)
})



test_that("Check whether fitting error is better than glmnet.",{
  nobs <- 1000
  xy <- createXY(nobs, 3, 3)
  lambda <- 1e-4
  rmse_aglm <- mean((predict(aglm(x=xy$x, x_UD=xy$x_UD, y=xy$y, lambda=lambda), newx=xy$x, newx_UD=xy$x_UD) - xy$y) ** 2)
  rmse_glmnet <- mean((predict(glmnet(x=xy$x, y=xy$y, lambda=lambda), newx=xy$x) - xy$y) ** 2)
  expect_true(rmse_aglm < rmse_glmnet)
})
