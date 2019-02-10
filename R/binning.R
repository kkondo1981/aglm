# utility functions for binning numerical data
# written by Kenji Kondo @ 2019/1/1


# Inner function used in executeBinning
isBinningFeasible <- function(x_vec) {
  return(class(x_vec) == "integer" | class(x_vec) == "numeric")
}


#' Create bins by Equal Width Binning
#'
#' @param left left value of the original interval.
#' @param right right value of the original interval.
#' @param nbin number of bins to be created.
#'
#' @return a numeric vector which indicates the boundaries of bins, with (number of bins + 1) elements.
#'
#' @export
#' @importFrom assertthat assert_that
createEqualWidthBins <- function(left, right, nbin){
  nbin <- as.integer(nbin)
  assert_that(length(nbin) == 1 & nbin > 1)

  breaks <- (0:nbin) * ((right - left) / nbin) + left
  return(breaks)
}


#' Create bins by Equal Freq Binning
#'
#' @param x_vec A reference integer or numeric vector to be binned.
#' @param nbin.max An integer value which indicates the maximum counts of bins.
#'   Note that this function makes `min(nbin.max, length(x_vec))` counts of bins.
#'
#' @return a numeric vector which indicates the boundaries of bins, with (number of bins + 1) elements.
#'
#' @export
#' @importFrom assertthat assert_that
createEqualFreqBins <- function(x_vec, nbin.max) {
  nbin.max <- as.integer(nbin.max)
  assert_that(nbin.max > 0 & length(x_vec) > 0)
 
  nbin <- min(nbin.max, length(x_vec))
  percents <- seq(0, 1, 1 / (nbin - 1))
  breaks <- unique(as.numeric(quantile(x_vec, percents)))

  return(breaks)
}


#' Execute binning for numerical data.
#'
#' @param x_vec an integer or numeric vector, to be binned.
#' @param breaks a numeric vector which indicates the boundaries of bins, of length (number of bins + 1).
#'   If NULL is set, evenly cut bins are generated and used.
#' @param nbin a number of bins which is used. Only used when `breaks` is not set.
#' @param allow_na a boolean value to tell whether all the elements should be binned or not (default is FALSE).
#'   If set TRUE, the leftmost and rightmost boundaries of bins are enhanced to -Inf and +Inf, respectively.
#' @param method used for specifying binning method. "freq": equal freq binning (default), "width": equal width binning.
#'
#' @return a list which has two members `labels` and `breaks`.
#' * `labels`: an integer vector of `length(x_vec)`.
#'   `(labels[i]==k)` indicates the i-th element of x_vec is in the k-th bin.
#' * `breaks`: a numeric vector which indicates the boundaries of bins, of length (number of bins + 1).
#'   This vector can be different from the `breaks` argument when `breaks` is not set or `allow_na` is FALSE.
#'
#' @export
#' @importFrom assertthat assert_that
executeBinning <- function(x_vec, breaks=NULL, nbin=20, allow_na=FALSE, method="freq") {
  # Check arguments
  assert_that(isBinningFeasible(x_vec))

  # If breaks is NULL, use even bins.
  if (is.null(breaks)) {
    left <- min(x_vec)
    right <- max(x_vec)
    breaks = createEvenBins(left, right, nbin)
  }

  # Enhance the leftmost and rightmost boundaries of bins if NA values are not allowed.
  if (!allow_na) {
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
  }

  # Calc labels for each element
  labels <- cut(x_vec, breaks=breaks, labels=FALSE)

  return(list(labels=labels, breaks=breaks))
}
