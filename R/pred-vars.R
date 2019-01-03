# S4 class for predictors used in aglm()
# written by Kenji Kondo @ 2019/1/2


#' S4 class for predictors
#'
#' @slot vars_info A list of lists. Each element represents one predictor and contains information
#'   of it and how to create its matrix representation from `data` slot.
#' @slot data A data.frame which contains numerical values for creation of matrix representation of predictors.
#'
#' @export
setClass("PredVars",
         representation=representation(vars_info="list", data="data.frame"))


#' Create a new PredVars object
#'
#' @param x (optional) An input matrix or data.frame of size (nobs, _). Each row should be an integer or numeric vector.
#'   All the values in `x` are interpreted as quantitative data if `UD_vars` is not given (NULL).
#'   For behaviours when `UD_vars` is given (not NULL), see the descriptions of `UD_vars`.
#'   When `x` is not given (NULL), `x_UD` should be given (not NULL).
#' @param x_UD (optional) An additional input matrix of size (nobs, _). Each row should be an integer, character,
#'   or factor object. All the values in `x_UD` are interpreted as qualitative data.
#' @param UD_vars (optional) An integer or character optional vector.
#'   If an integer vector is given, all the values of `x[, UD_vars]` are interpreted as qualitative data,
#'   Else if a character vector is given, all the values of `x[, names(x) %in% UD_vars]` are interpreted as
#'   qualitative data instead.
#'
#' @return A new PredVars object which hold entire information of `x` and `x_UD`
#'
#' @export
#' @importFrom assertthat assert_that
newPredVars <- function(x=NULL, x_UD=NULL, UD_vars=NULL) {
  # Check and process arguments
  assert_that(!is.null(x) | !is.null(x_UD))
  if (!is.null(x)) {
    assert_that(class(x) == "matrix" | class(x) == "data.frame")
    assert_that(dim(x)[2] > 0)
  }
  if (!is.null(x_UD)) {
    assert_that(class(x_UD) == "matrix" | class(x_UD) == "data.frame")
    if (!is.null(x)) assert_that(dim(x_UD)[1] == dim(x)[1])
    assert_that(dim(x_UD)[2] > 0)
  }
  assert_that(is.null(x_UD) | is.null(UD_vars))


  # Combine all the variables into x_all and provide UD_vars for x_all
  x_all <- NULL
  if (is.null(UD_vars) & is.null(x_UD)) {
    # Create UD_vars automatically if both UD_vars and x_UD are not given
    x_all <- data.frame(x)
    UD_vars <- (1:dim(x)[2])[(sapply(x, class) == "factor") | (sapply(x, class) == "character")]
  } else if (!is.null(UD_vars)) {
    # if UD_vars is not integer, convert it
    if (class(UD_vars) == "numeric") UD_vars <- as.integer(UD_vars)
    else if (class(UD_vars) == "character") UD_vars <- (1:dim(x)[2])[names(x) %in% UD_vars]
    x_all <- data.frame(x)
  } else if (!is.null(x_UD)) {
    # If x_UD is given , combine x_UD into x and calculate UD_vars for the whole matrix
    if (is.null(x)) {
      x_all <- data.frame(x_UD)
      UD_vars <- 1:dim(x_UD)[2]
    } else {
      x_all <- data.frame(x, x_UD)
      UD_vars <- (dim(x)[2] + 1):(dim(x_all)[2])
    }
  } else {  # never comes
    assert_that(FALSE)
  }
  assert_that(class(x_all) == "data.frame")


  # Calculate data size
  nobs <- dim(x_all)[1]
  nvar <- dim(x_all)[2]
  assert_that(nobs > 0 & nvar > 0)


  # Create a variable information list and design matrix passed to glmnet
  OD_vars <- if (length(UD_vars) > 0) (1:nvar)[-UD_vars] else 1:nvar
  vars_info <- list()

  # Get information of variable for each column of x in OD_vars
  if (length(OD_vars) > 0) {
    for (i in OD_vars) {
      var_info <- list(idx=i,
                       name=names(x_all)[i],
                       type="O",
                       data_column=i,
                       mu=mean(x_all[, i]),
                       sigma=sd(x_all[, i]),
                       dummy_info=getODummyMatForOneVec(x_all[, i], only_info=TRUE))
      vars_info[[i]] <- var_info
    }
  }

  # Get information of variable for each column of x in UD_vars
  if (length(UD_vars) > 0) {
    for (i in UD_vars) {
      var_info <- list(idx=i,
                       name=names(x_all)[i],
                       type="U",
                       data_column=i,
                       dummy_info=getUDummyMatForOneVec(x_all[, i], only_info=TRUE))
      vars_info[[i]] <- var_info
    }
  }

  new("PredVars", vars_info=vars_info, data=x_all)
}


#' Get design-matrix representation of PredVars objects
#'
#' @param preds A PredVars object
#' @param standardize_qualitative_vars A boolean value indicating qualitative values should be standardized.
#'   Note that this option does not affect creations of dummy values (both O-dummies and U-dummies).
#'
#' @return A data.frame which represents the matrix representation of `preds`.
#'
#' @export
#' @importFrom assertthat assert_that
getDesignMatrix <- function(preds, standardize_qualitative_vars=TRUE) {
  # Check arguments
  assert_that(class(preds) == "PredVars")

  # Data size
  nobs <- dim(preds@data)[1]
  nvar <- length(preds@vars_info)
  x <- NULL

  for (i in 1:nvar) {
    var_info <- preds@vars_info[[i]]
    z <- NULL
    if (var_info$type == "O") {
      z_raw <- matrix(preds@data[, var_info$data_column], ncol=1)
      if (standardize_qualitative_vars & var_info$sigma > 0)
        z_raw <- (z_raw - var_info$mu) / var_info$sigma
      colnames(z_raw) <- var_info$name
      z_OD <- getODummyMatForOneVec(preds@data[, var_info$data_column],
                                    breaks=var_info$dummy_info$breaks)$dummy_mat
      colnames(z_OD) <- paste0(var_info$name, "_dummy_", seq(dim(z_OD)[2]))
      z <- cbind(z_raw, z_OD)
    }else if (var_info$type == "U") {
      z <- getUDummyMatForOneVec(preds@data[, var_info$data_column],
                                 levels=var_info$dummy_info$levels,
                                 drop_last=var_info$dummy_info$drop_last)$dummy_mat
      colnames(z) <- paste0(var_info$name, "_dummy_", seq(dim(z)[2]))
    } else {
      assert_true(FALSE)  # never expects to come here
    }

    if (i == 1) x <- z
    else x <- cbind(x, z)
  }

  return(x)
}
