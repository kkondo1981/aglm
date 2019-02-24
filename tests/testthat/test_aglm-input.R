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

test_that("Check returned values of newInput() for each input type", {
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


test_that("Check add_xxx flags of newInput()", {
  x <- newInput(createX(10, 1, 1, 1, 1), add_intersection_columns=FALSE)
  expect_equal(length(x@vars_info), 4)

  x <- newInput(createX(10, 1, 1, 1, 1), add_linear_columns=FALSE, add_intersection_columns=FALSE)
  expect_true(all(sapply(x@vars_info, function(var) {!var$use_linear})))

  x <- newInput(createX(10, 1, 1, 1, 1), add_OD_columns_of_qualitatives=FALSE, add_intersection_columns=FALSE)
  expect_true(all(sapply(x@vars_info, function(var) {var$type=="quan" | !var$use_OD})))
})

test_that("Check bins_list of newInput()", {
  bins_list <- list(c(0, 1, 2))
  x <- newInput(createX(10, 0, 5, 0, 0), bins_list=bins_list)
  expect_equal(x@vars_info[[1]]$OD_info$breaks, bins_list[[1]])

  bins_names <- list(3)
  x <- newInput(createX(10, 0, 5, 0, 0), bins_list=bins_list, bins_names=bins_names)
  expect_equal(x@vars_info[[3]]$OD_info$breaks, bins_list[[1]])

  bins_names <- list("Num5")
  x <- newInput(createX(10, 0, 5, 0, 0), bins_list=bins_list, bins_names=bins_names)
  #expect_equal(x@vars_info[[5]]$OD_info$breaks, bins_list[[1]])
})

test_that("Check return values of getDesignMatrix()", {
  x_int <- newInput(createX(10, 1, 0, 0, 0), add_intersection_columns=FALSE)
  mat_int <- getDesignMatrix(x_int)
  #print("")
  #print(t(x_int@data))
  #print(mat_int)
  expect_equal(mat_int[,1], x_int@data[,1])
  expect_equal(dim(mat_int), c(10, dim(getODummyMatForOneVec(mat_int[,1])$dummy_mat)[2] + 1))

  x_num <- newInput(createX(10, 0, 1, 0, 0), add_intersection_columns=FALSE)
  mat_num <- getDesignMatrix(x_num)
  #print("")
  #print(t(x_num@data))
  #print(mat_num)
  expect_equal(mat_num[,1], x_num@data[,1])
  expect_equal(dim(mat_num), c(10, dim(getODummyMatForOneVec(mat_num[,1])$dummy_mat)[2] + 1))

  x_ord <- newInput(createX(10, 0, 0, 1, 0), add_intersection_columns=FALSE)
  mat_ord <- getDesignMatrix(x_ord)
  #print("")
  #print(t(x_ord@data))
  #print(mat_ord)
  expect_equal(dim(mat_ord), c(10,
                               dim(getODummyMatForOneVec(x_ord@data[,1])$dummy_mat)[2]
                               + dim(getUDummyMatForOneVec(x_ord@data[,1], drop_last=FALSE)$dummy_mat)[2]))

  x_fac <- newInput(createX(10, 0, 0, 0, 1), add_intersection_columns=FALSE)
  mat_fac <- getDesignMatrix(x_fac)
  #print("")
  #print(t(x_fac@data))
  #print(mat_fac)
  expect_equal(dim(mat_fac), c(10, dim(getUDummyMatForOneVec(x_fac@data[,1], drop_last=FALSE)$dummy_mat)[2]))

  x_all <- newInput(data.frame(x_int@data, x_num@data, x_ord@data, x_fac@data), add_intersection_columns=FALSE)
  mat_all <- getDesignMatrix(x_all)
  expect_equal(mat_all, cbind(mat_int, mat_num, mat_ord, mat_fac))


  ## Check intersection columns
  x_inter <- newInput(data.frame(x_int@data,  x_fac@data), add_intersection_columns=TRUE)
  mat_inter <- getDesignMatrix(x_inter)
  a <- dim(mat_int)[2] + dim(mat_fac)[2]
  b <- dim(x_int@data)[2] + dim(mat_fac)[2]
  expect_equal(dim(mat_inter), c(10, a + b * (b - 1) / 2))
})
