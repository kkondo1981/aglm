context("aglm-input")
library(aglm)


createX <- function(nobs, nvar_int, nvar_numeric, nvar_ordered, nvar_factor, seed=12345) {
  set.seed(seed)
  nobs <- nobs
  nvar <- nvar_int + nvar_numeric + nvar_ordered + nvar_factor

  data <- list()
  if (nvar_int > 0) for (i in 1:nvar_int) data[[paste0("Int", i)]] <- sample(1:10, size=nobs, replace=TRUE)
  if (nvar_numeric > 0) for (i in 1:nvar_numeric) data[[paste0("Num", i)]] <- rnorm(nobs)
  if (nvar_ordered > 0) for (i in 1:nvar_ordered) data[[paste0("Ord", i)]] <- ordered(sample(1:5, size=nobs, replace=TRUE))
  if (nvar_factor > 0) for (i in 1:nvar_factor) data[[paste0("Fac", i)]] <- factor(sample(c("A", "B", "C"), nobs, replace=TRUE))

  return(data.frame(data))
}

test_that("Check returned values of createX() for each input types", {
  x <- newInput(createX(10, 1, 1, 1, 1))

  expect_equal(x@vars_info[[1]]$id, 1)
  expect_equal(x@vars_info[[1]]$data_column_idx, 1)
  expect_equal(x@vars_info[[1]]$type, "quan")
  expect_equal(x@vars_info[[1]]$use_linear, TRUE)
  expect_equal(x@vars_info[[1]]$use_UD, FALSE)
  expect_equal(x@vars_info[[1]]$use_OD, TRUE)
  expect_true(!is.null(x@vars_info[[1]]$OD_info))
  expect_true(is.null(x@vars_info[[1]]$UD_info))

  expect_equal(x@vars_info[[2]]$id, 2)
  expect_equal(x@vars_info[[2]]$data_column_idx, 2)
  expect_equal(x@vars_info[[2]]$type, "quan")
  expect_equal(x@vars_info[[2]]$use_linear, TRUE)
  expect_equal(x@vars_info[[2]]$use_UD, FALSE)
  expect_equal(x@vars_info[[2]]$use_OD, TRUE)
  expect_true(!is.null(x@vars_info[[2]]$OD_info))
  expect_true(is.null(x@vars_info[[2]]$UD_info))

  expect_equal(x@vars_info[[3]]$id, 3)
  expect_equal(x@vars_info[[3]]$data_column_idx, 3)
  expect_equal(x@vars_info[[3]]$type, "qual")
  expect_equal(x@vars_info[[3]]$use_linear, FALSE)
  expect_equal(x@vars_info[[3]]$use_UD, TRUE)
  expect_equal(x@vars_info[[3]]$use_OD, TRUE)
  expect_true(!is.null(x@vars_info[[3]]$UD_info))
  expect_true(!is.null(x@vars_info[[3]]$OD_info))

  expect_equal(x@vars_info[[4]]$id, 4)
  expect_equal(x@vars_info[[4]]$data_column_idx, 4)
  expect_equal(x@vars_info[[4]]$type, "qual")
  expect_equal(x@vars_info[[4]]$use_linear, FALSE)
  expect_equal(x@vars_info[[4]]$use_UD, TRUE)
  expect_equal(x@vars_info[[4]]$use_OD, FALSE)
  expect_true(!is.null(x@vars_info[[4]]$UD_info))
  expect_true(is.null(x@vars_info[[4]]$OD_info))
})


test_that("Check add_xxx flags", {
  x <- newInput(createX(10, 1, 1, 1, 1), add_intersection_columns=FALSE)
  expect_equal(length(x@vars_info), 4)

  x <- newInput(createX(10, 1, 1, 1, 1), add_linear_columns=FALSE, add_intersection_columns=FALSE)
  expect_true(all(sapply(x@vars_info, function(var) {!var$use_linear})))

  x <- newInput(createX(10, 1, 1, 1, 1), add_OD_columns_of_qualitatives=FALSE, add_intersection_columns=FALSE)
  expect_true(all(sapply(x@vars_info, function(var) {var$type=="quan" | !var$use_OD})))
})
