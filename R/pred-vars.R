# S4 class for predictors used in aglm()
# written by Kenji Kondo @ 2019/1/2


#' S4 class for predictors
#'
#' @slot vars_info A list of list. Each element represents one predictor and contains various informations of it.
#' @slot data A data.frame which contains original data itself.
setClass("PredVars",
         representation=representation(vars_info="list", data="data.frame"))


#' Create a new PredVars object
#'
#' @param x An input matrix or data.frame, each column of which represents one predictive variable (or explanatory variable).
#'   Columns of `x` can contain both quantitative values (as integer or numeric) and qualitative values (as character or factor), but which variables are quantitative or qualitative is usually to be instructed by users.
#'   * The common way is to give both `x` and `x_UD` arguments separately.
#'     In this case, all the columns of `x` are treated as quantitative and all the columns of `x_UD` are treated as qualitative.
#'   * Another way is to give only `x` but provide `UD_vars` for instructing which columns of `x` should be treated as qualitative.
#'   * If only `x` is given and neither of `x_UD` nor `UD_vars` is not provided, all the columns of `x` are treated as quantitative.
#'   * Moreover, users can give only `x_UD`. In this case, nothing is considered as quantitative.
#' @param x_UD An input matrix or data.frame for qualitative data. See the descriptions of `x` for more details.
#' @param UD_vars An integer or character optional vector.
#'   If an integer vector is given, all the values of `x[, UD_vars]` are interpreted as qualitative.
#'   Else if a character vector is given, all the values of `x[, names(x) %in% UD_vars]` are interpreted as qualitative instead.
#'   See the descriptions of `x` for more details.
#' @param append_interaction_vars A boolean flag. If `TRUE`, predictive variables of interaction effects are added to the returned `PredVars` object.
#'   Each of such variable represents an interaction effect between one pair of columns of `x` and `x_UD`, and total number of variables are `nvar * (nvar - 1) /2` where `nvar=dim(x)[2] + dim(x_UD)[2]`.
#'   The default value is `TRUE`.
#'
#' @return A new PredVars object which hold entire information of `x` and `x_UD`
#'
#' @importFrom assertthat assert_that
newPredVars <- function(x=NULL, x_UD=NULL, UD_vars=NULL, append_interaction_vars=TRUE) {
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

  # Add interaction variables of variable pairs if instructed
  if (append_interaction_vars & nvar > 1) {
    idx <- length(vars_info)
    for (i in 1:(nvar - 1)) {
      for (j in (i + 1):nvar) {
        idx <- idx + 1
        var_info1 <- vars_info[[i]]
        var_info2 <- vars_info[[j]]
        var_info <- list(idx=idx,
                         name=paste0(var_info1$name, "_x_", var_info2$name),
                         type="I",
                         var_idx1=i,
                         var_idx2=j)
        vars_info[[idx]] <- var_info
      }
    }
  }

  new("PredVars", vars_info=vars_info, data=x_all)
}


# Functions for inner use
getMatrixRepresentation <- function(preds, idx, standardize_quantitative_vars) {
  var_info <- preds@vars_info[[idx]]
  z <- NULL
  if (var_info$type == "O") {
    z_raw <- matrix(preds@data[, var_info$data_column], ncol=1)
    if (standardize_quantitative_vars & var_info$sigma > 0)
      z_raw <- (z_raw - var_info$mu) / var_info$sigma
    colnames(z_raw) <- var_info$name
    z_OD <- getODummyMatForOneVec(preds@data[, var_info$data_column],
                                  breaks=var_info$dummy_info$breaks)$dummy_mat
    colnames(z_OD) <- paste0(var_info$name, "_dummy_", seq(dim(z_OD)[2]))
    z <- cbind(z_raw, z_OD)
  } else if (var_info$type == "U") {
    z <- getUDummyMatForOneVec(preds@data[, var_info$data_column],
                               levels=var_info$dummy_info$levels,
                               drop_last=var_info$dummy_info$drop_last)$dummy_mat
    colnames(z) <- paste0(var_info$name, "_dummy_", seq(dim(z)[2]))
  } else if (var_info$type == "I") {
    # Get matrix representations of two variables
    z1 <- getMatrixRepresentation(preds, var_info$var_idx1, standardize_quantitative_vars)
    z2 <- getMatrixRepresentation(preds, var_info$var_idx2, standardize_quantitative_vars)

    # Discard O-dummies
    if (preds@vars_info[[var_info$var_idx1]]$type == "O")
      z1 <- matrix(z1[, 1], ncol=1)
    if (preds@vars_info[[var_info$var_idx2]]$type == "O")
      z2 <- matrix(z2[, 1], ncol=1)

    # Create matrix representation of intarction
    nrow <- dim(z1)[1]
    ncol1 <- dim(z1)[2]
    ncol2 <- dim(z2)[2]
    assert_that(dim(z2)[1] == nrow)
    z <- matrix(0, nrow, ncol1 * ncol2)
    nm <- character(ncol1 * ncol2)
    for (i in 1:ncol1) {
      for (j in 1:ncol2) {
        ij <- (i - 1) * ncol2 + j
        z[, ij] <- z1[, i] * z2[, j]
        nm[ij] <- ifelse(ncol1 * ncol2 == 1, var_info$name, paste0(var_info$name, "_", i, "_", j))
      }
    }
    colnames(z) <- nm
  } else {
    assert_true(FALSE)  # never expects to come here
  }
  return(z)
}


#' Get design-matrix representation of PredVars objects
#'
#' @param preds A PredVars object
#' @param standardize_quantitative_vars A boolean value indicating quantitative values should be standardized.
#'   Note that this option does not affect creations of dummy values (both O-dummies and U-dummies).
#'
#' @return A data.frame which represents the matrix representation of `preds`.
#'
#' @importFrom assertthat assert_that
getDesignMatrix <- function(preds, standardize_quantitative_vars=TRUE) {
  # Check arguments
  assert_that(class(preds) == "PredVars")

  # Data size
  nobs <- dim(preds@data)[1]
  nvar <- length(preds@vars_info)
  x <- NULL

  for (i in 1:nvar) {
    z <- getMatrixRepresentation(preds, i, standardize_quantitative_vars)
    if (i == 1) x <- z
    else x <- cbind(x, z)
  }

  return(x)
}
