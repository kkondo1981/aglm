# utility functions for get dummy variables from various data
# written by Kenji Kondo @ 2019/1/1

#' Get U-dummy matrix for one-dimensional vector
#'
#' @param x_vec non-numeric vector to be converted into dummy matrix.
#' @param drop_last boolean value. If TRUE, the last column of dummy matrix is dropped to avoid colinearity (default is TRUE).
#'
#' @return a list with two members `levels` and `dummy_mat`.
#' * `levels`: a character vector. unique set of possible values. sorted.
#' * `dummy_mat`: an integer matrix with size (length of `x_vec`, length of `levels` or minus 1 when `drop_last=TRUE`).
#'   `dummy_mat[i, j]` is 1 if and only if `x_vec[i] == levels[j]`, and 0 otherwise.
#'
#' @export
#' @importFrom assertthat assert_that
getUDummyMatForOneVec <- function(x_vec, drop_last=TRUE) {
  # Check arguments. numerical vectors are not allowed.
  assert_that(class(x_vec) == "integer" | class(x_vec) == "character" | class(x_vec) == "factor")

  # Convert x_vec into factor if not
  if (class(x_vec) != "factor") {
    x_vec <- as.factor(x_vec)
  }

  # create dummy matrix for x_vec
  int_labels <- as.integer(labels(x_vec))
  nrow <- length(x_vec)
  ncol <- length(levels(x_vec))
  if (drop_last) ncol <- ncol - 1
  dummy_mat <- 1 * (int_labels == t(matrix(1:ncol, ncol, nrow)))

  return(list(levels=levels(x_vec), dummy_mat=dummy_mat))
}


#' Get O-dummy matrix for one-dimensional vector
#'
#' @param x_vec integer or numeric vector to be converted into dummy matrix.
#' @param breaks a numeric vector which indicates the boundaries of bins, of length (number of bins + 1).
#'   If NULL is set, evenly cut bins are generated and used.
#' @param nbin a number of bins which is used. Only used when `breaks` is not set.
#'
#' @return a list with two members `breaks` and `dummy_mat`.
#' * `breaks`: a numeric vector which indicates the boundaries of bins, of length (number of bins + 1).
#' * `dummy_mat`: an integer matrix with size (length of `x_vec`, length of `breaks` minus 1).
#'   `dummy_mat[i, j]` is 1 if and only if `breaks[i] < x_vec[i] <= breaks[i+1]`, and 0 otherwise.
#'   Note that, in case where `x_vec[i]` is outside of `(breaks[1], breaks[length(breaks)]]`,
#'   `x_vec[i]` is considered to be in the first bin if `x_vec[i] <= breaks[1]`, and
#'   be in the last bin if `x_vec[i] > breaks[length(breaks)]`.
#'
#' @export
#' @importFrom assertthat assert_that
getODummyMatForOneVec <- function(x_vec, breaks=NULL, nbin=20) {
  # Check arguments. only integer or numerical vectors are allowed.
  assert_that(class(x_vec) == "integer" | class(x_vec) == "numeric")

  # Execute binning
  binned_x <- executeBinning(x_vec, breaks=breaks, nbin=nbin, allow_na=FALSE)

  # create dummy matrix for x_vec
  nrow <- length(x_vec)
  ncol <- length(binned_x$breaks) - 1
  dummy_mat <- 1 * (binned_x$labels <= t(matrix(1:ncol, ncol, nrow)))

  return(list(breaks=binned_x$breaks, dummy_mat=dummy_mat))
}
