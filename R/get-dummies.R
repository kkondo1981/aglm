# utility functions for get dummy variables from various data
# written by Kenji Kondo @ 2019/1/1

#' Get U-dummy matrix for one-dimensional vector
#'
#' @param x_vec non-numeric vector to be converted into dummy matrix.
#' @drop_last boolean value. If TRUE, the last column of dummy matrix is dropped to avoid colinearity (default is TRUE).
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

  #create dummy matrix for x_vec
  int_labels <- as.integer(labels(x_vec))
  nrow <- length(x_vec)
  ncol <- length(levels(x_vec))
  if (drop_last) ncol <- ncol - 1
  dummy_mat <- 1 * (int_labels == t(matrix(1:ncol, ncol, nrow)))

  return(list(levels=levels(x_vec), dummy_mat=dummy_mat))
}
