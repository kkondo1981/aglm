# utility functions for get dummy variables from various data
# written by Kenji Kondo @ 2019/1/1

#' Get U-dummy matrix for one-dimensional vector
#'
#' @param x_vec A non-numeric vector to be converted into dummy matrix.
#' @param levels A character vector indicates unique set of possible values.
#'   If NULL, all the unique values of `x_vec` are used.
#' @param drop_last A boolean value. If TRUE, the last column of dummy matrix is dropped to avoid colinear
#' @param only_info A bboolean value. If TRUE, actual creation of dummy matrix is omitted.
#'
#' @return a list with two members `levels` and `dummy_mat`.
#' * `levels`: Same as input
#' * `drop_last`: Same as input
#' * `dummy_mat`: An integer matrix with size (length of `x_vec`, length of `levels` or minus 1 when `drop_last=TRUE`).
#'   `dummy_mat[i, j]` is 1 if and only if `x_vec[i] == levels[j]`, and 0 otherwise.
#'   Omitted if `only_info=TRUE`
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


#' Get O-dummy matrix for one-dimensional vector
#'
#' @param x_vec An integer or numeric vector to be converted into dummy matrix.
#' @param breaks A numeric vector which indicates the boundaries of bins, of length (number of bins + 1).
#'   If NULL, evenly cut bins are automatically generated and used.
#' @param nbin.max A maximum number of bins which is used. Only used when `breaks` is not set.
#' @param only_info A boolean value. If TRUE, actual creation of dummy matrix is omitted.
#'
#' @return a list with two members `breaks` and `dummy_mat`.
#' * `breaks`: Same as input
#' * `dummy_mat`: An integer matrix with size (length of `x_vec`, length of `breaks` minus 1).
#'   `dummy_mat[i, j]` is 1 if and only if `breaks[i] < x_vec[i] <= breaks[i+1]`, and 0 otherwise.
#'   Note that, in case where `x_vec[i]` is outside of `(breaks[1], breaks[length(breaks)]]`,
#'   `x_vec[i]` is considered to be in the first bin if `x_vec[i] <= breaks[1]`, and
#'   be in the last bin if `x_vec[i] > breaks[length(breaks)]`.
#'   Omitted if `only_info=TRUE`
#'
#' @export
#' @importFrom assertthat assert_that
getODummyMatForOneVec <- function(x_vec, breaks=NULL, nbin.max=100, only_info=FALSE) {
  # Check arguments. only integer or numerical or ordered vectors are allowed.
  assert_that(is.integer(x_vec) | is.numeric(x_vec) | is.ordered(x_vec))

  # Execute binning
  binned_x <- executeBinning(x_vec, breaks=breaks, nbin.max=nbin.max, allow_na=FALSE)

  # create dummy matrix for x_vec
  nrow <- length(x_vec)
  ncol <- length(binned_x$breaks) - 1
  dummy_mat <- 1 * (binned_x$labels <= t(matrix(1:ncol, ncol, nrow)))

  if (only_info) return(list(breaks=binned_x$breaks))
  else return(list(breaks=binned_x$breaks, dummy_mat=dummy_mat))
}
