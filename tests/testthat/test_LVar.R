context("LVar")
library(aglm)

test_that("getLVarMatForOneVec()'s outputs are correct.", {
  expect_equal(getLVarMatForOneVec(1:3)$dummy_mat, matrix(c(0, 1, 2, 1, 0, 1), 3, 2))
  expect_equal(getLVarMatForOneVec(c(1, 1.5, 2, 2.3, 3), breaks=1:3)$dummy_mat,
               matrix(c(0, 0.5, 1, 1.3, 2, 1, 0.5, 0, 0.3, 1), 5, 2))
  expect_equal(getLVarMatForOneVec(c(1, 1.5, 2, 2.3, 3), breaks=c(0, 1.8, 4))$dummy_mat,
               matrix(c(1, 1.5, 2, 2.3, 3, 0.8, 0.3, 0.2, 0.5, 1.2), 5, 2))
})

createX <- function(nobs, nvar_numeric, seed=12345) {
  set.seed(seed)
  nobs <- nobs

  data <- list()
  if (nvar_numeric > 0) for (i in 1:nvar_numeric) data[[paste0("Num", i)]] <- rnorm(nobs)

  return(data.frame(data))
}

test_that("Check newInput() for L-Variable", {
  x <- newInput(createX(10, 1), use_LVar=TRUE)

  expect_equal(x@vars_info[[1]]$id, 1)
  expect_equal(x@vars_info[[1]]$data_column_idx, 1)
  expect_equal(x@vars_info[[1]]$type, "quan")
  expect_equal(x@vars_info[[1]]$use_linear, FALSE)
  expect_equal(x@vars_info[[1]]$use_UD, FALSE)
  expect_equal(x@vars_info[[1]]$use_OD, FALSE)
  expect_equal(x@vars_info[[1]]$use_LV, TRUE)
  expect_true(is.null(x@vars_info[[1]]$OD_info))
  expect_true(is.null(x@vars_info[[1]]$UD_info))
  expect_true(!is.null(x@vars_info[[1]]$LV_info))

  mat_num <- getDesignMatrix(x)
  expect_equal(mat_num[,1], x@data[,1]-min(x@data[,1]))
  expect_equal(dim(mat_num), c(10, dim(getLVarMatForOneVec(mat_num[,1])$dummy_mat)[2]))

  bins_list <- list(c(0, 1, 2))
  x <- newInput(createX(10, 1), use_LVar=TRUE, bins_list=bins_list)
  expect_equal(x@vars_info[[1]]$LV_info$breaks, bins_list[[1]])
})
