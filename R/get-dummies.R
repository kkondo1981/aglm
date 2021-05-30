#' Create a U-dummy matrix for one variable
#'
#' @param x_vec A vector representing original variable.
#'   The class of `x_vec` should be one of `integer`, `character`, or `factor`.
#' @param levels A character vector representing values of `x_vec` used to create U-dummies.
#'   If `NULL`, all the unique values of `x_vec` are used to create dummies.
#' @param drop_last If `TRUE`, the last column of the resulting matrix is dropped to avoid multicollinearity.
#' @param only_info If `TRUE`, only information fields of returned values are filled and no dummy matrix is returned.
#'
#' @return A list with the following fields:
#' * `levels`: Same as input.
#' * `drop_last`: Same as input.
#' * `dummy_mat`: The created U-dummy matrix (only if `only_info=FALSE`).
#'
#' @author Kenji Kondo
#'
#' @export
#' @importFrom assertthat assert_that
getUDummyMatForOneVec <- function(x_vec, levels=NULL, drop_last=TRUE, only_info=FALSE) {
  # Check arguments. numerical vectors are not allowed.
  assert_that(is.integer(x_vec) | is.character(x_vec) | is.factor(x_vec))

  # Create factor. Note that if x_vec is itself factor and levels is not specified, do not need nothing.
  if (!is.factor(x_vec) | !is.null(levels)) {
    x_vec <- as.character(x_vec)
    if (is.null(levels)) x_vec <- factor(x_vec)
    else x_vec <- factor(x_vec, levels=levels)
  }

  # create dummy matrix for x_vec
  int_labels <- as.integer(x_vec)
  nrow <- length(x_vec)
  ncol <- length(levels(x_vec))
  if (drop_last) ncol <- ncol - 1
  dummy_mat <- 1 * (int_labels == t(matrix(1:ncol, ncol, nrow)))

  if (only_info) return(list(levels=levels(x_vec), drop_last=drop_last))
  else return(list(levels=levels(x_vec), drop_last=drop_last, dummy_mat=dummy_mat))
}


#' Create a O-dummy matrix for one variable
#'
#' @param x_vec A numeric vector representing original variable.
#' @param breaks A numeric vector representing breaks of bins (If `NULL`, automatically generated).
#' @param nbin.max The maximum number of bins (used only if `breaks=NULL`).
#' @param only_info If `TRUE`, only information fields of returned values are filled and no dummy matrix is returned.
#' @param dummy_type Used to control the shape of linear combinations obtained by O-dummies for quantitative variables (deprecated).
#'
#' @return a list with the following fields:
#' * `breaks`: Same as input
#' * `dummy_mat`: The created O-dummy matrix (only if `only_info=FALSE`).
#'
#' @author Kenji Kondo
#'
#' @export
#' @importFrom assertthat assert_that
getODummyMatForOneVec <- function(x_vec, breaks=NULL, nbin.max=100, only_info=FALSE, dummy_type=NULL) {
  # Check arguments. only integer or numerical or ordered vectors are allowed.
  assert_that(is.integer(x_vec) | is.numeric(x_vec) | is.ordered(x_vec))
  if (is.null(dummy_type)) {
    if (is.ordered(x_vec)) dummy_type <- "J"
    else dummy_type <- "C"
  }

  # Execute binning
  binned_x <- executeBinning(x_vec, breaks=breaks, nbin.max=nbin.max)

  # create dummy matrix for x_vec
  if (dummy_type == "C") {
    nrow <- length(x_vec)
    ncol <- length(binned_x$breaks) - 1
    X <- matrix(x_vec, nrow, ncol)
    B0 <- t(matrix(binned_x$breaks[1:ncol], ncol, nrow))
    B1 <- t(matrix(binned_x$breaks[-1], ncol, nrow))
    dummy_mat <- (X - B0) / (B1 - B0)
    dummy_mat[dummy_mat <= 0] <- 0
    dummy_mat[dummy_mat >= 1] <- 1
  } else if (dummy_type == "J") {
    nrow <- length(x_vec)
    ncol <- length(binned_x$breaks)
    dummy_mat <- 1 * (binned_x$labels > t(matrix(1:ncol, ncol, nrow)))
  } else {
    assert_that(FALSE, msg="dummy_type must be \"C\" or \"J\".")
  }

  if (only_info) return(list(breaks=binned_x$breaks))
  else return(list(breaks=binned_x$breaks, dummy_mat=dummy_mat))
}


#' Create L-variable matrix for one variable
#'
#' @param x_vec A numeric vector representing original variable.
#' @param breaks A numeric vector representing breaks of bins (If `NULL`, automatically generated).
#' @param nbin.max The maximum number of bins (used only if `breaks=NULL`).
#' @param only_info If `TRUE`, only information fields of returned values are filled and no dummy matrix is returned.
#'
#' @return a list with the following fields:
#' * `breaks`: Same as input
#' * `dummy_mat`: The created L-variable matrix (only if `only_info=FALSE`).
#'
#' @author Kenji Kondo
#'
#' @export
#' @importFrom assertthat assert_that
getLVarMatForOneVec <- function(x_vec, breaks=NULL, nbin.max=100, only_info=FALSE) {
  # Check arguments. only integer or numerical or ordered vectors are allowed.
  assert_that(is.integer(x_vec) | is.numeric(x_vec) | is.ordered(x_vec))

  # Execute binning
  binned_x <- executeBinning(x_vec, breaks=breaks, nbin.max=nbin.max)

  # create dummy matrix for x_vec
  nrow <- length(x_vec)
  ncol <- length(binned_x$breaks) - 2
  if (ncol < 1) {
    dummy_mat <- NULL
  } else {
    X <- matrix(x_vec, nrow, ncol)
    B0 <- t(matrix(binned_x$breaks[2:(ncol + 1)], ncol, nrow))
    dummy_mat <- abs(X - B0)
  }

  if (only_info) return(list(breaks=binned_x$breaks))
  else return(list(breaks=binned_x$breaks, dummy_mat=dummy_mat))
}
