# Creation of aglm.input object
# written by Kenji Kondo @ 2019/1/1


#' Create aglm.input object
#'
#' @param x input matrix or data.frame of size (nobs, _). Each row shold be an integer or numeric vector.
#' @param x_UD additional and optional input matrix of size (nobs, _), whose columns are replaced with U-dummy
#'   matrices before fitting. Each row of `x_UD` should be an integer, character, or factor vector.
#' @param UD_vars integer or character optional vector. If an integer vector is given, this function replaces
#'   columns whose indices are in `UD_vars` with U-dummy matrices before fitting. If a character vector is given,
#'   this function replaces each column whose name is in `UD_vars` with U-dummy matrices before fitting.
#'   `UD_vars` is ignored when `x_UD` is not NULL.
#'   If both `UD_vars` and `x_UD` are not given (NULL values), columns with character types or factor types are
#'   automatically handled as in `UD_vars`.
#'
#' @return a list with two elements `x_all` and `vars_info`
#'  * `x_all`: a data.frame which hold the whole data of x and x_UD
#'  * `vars_info`: a list of length dim(x_all)[2]. `vars_info[[i]]` represents some information of `x_all[, i]`.
#'
#' @export
#' @importFrom assertthat assert_that
createAglmInput <- function(x=NULL, x_UD=NULL, UD_vars=NULL) {
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
                       dummy_info=getUDummyMatForOneVec(x_all[, i], only_info=TRUE))
      vars_info[[i]] <- var_info
    }
  }

  ret <- list(x_all=x_all, vars_info=vars_info)
  class(ret) <- "aglm.input"
  return(ret)
}
