# Inner function used in executeBinning
isBinningFeasible <- function(x_vec) {
  return(is.integer(x_vec) | is.numeric(x_vec) | is.ordered(x_vec))
}


#' Create bins (equal width binning)
#'
#' @param left The leftmost value of the interval to be binned.
#' @param right The rightmost value of the interval to be binned.
#' @param nbin The number of bins.
#'
#' @return A numeric vector representing breaks obtained by binning.
#'
#' @author Kenji Kondo
#'
#' @export
#' @importFrom assertthat assert_that
createEqualWidthBins <- function(left, right, nbin){
  nbin <- as.integer(nbin)
  assert_that(length(nbin) == 1 & nbin > 1)

  breaks <- (0:(nbin - 1)) * ((right - left) / (nbin - 1)) + left
  return(breaks)
}


#' Create bins (equal frequency binning)
#'
#' @param x_vec  A numeric vector, whose quantiles are used as breaks.
#' @param nbin.max The maximum number of bins.
#'
#' @return A numeric vector representing breaks obtained by binning.
#'   Note that the number of bins is equal to `min(nbin.max, length(x_vec))`.
#'
#' @author Kenji Kondo
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stats quantile
createEqualFreqBins <- function(x_vec, nbin.max) {
  nbin.max <- as.integer(nbin.max)
  assert_that(nbin.max > 1 & length(x_vec) > 0)

  nbin <- min(nbin.max, length(x_vec))
  percents <- seq(0, 1, 1 / (nbin - 1))
  if (is.ordered(x_vec)) {
    breaks <- unique(quantile(x_vec, percents, type=1))
    names(breaks) <- NULL
  } else { # integer or numeric cases
    breaks <- unique(as.numeric(quantile(x_vec, percents)))
  }

  return(breaks)
}


#' Binning the data to given bins.
#'
#' @param x_vec The data to be binned.
#' @param breaks A numeric vector representing breaks of bins (If `NULL`, automatically generated).
#' @param nbin.max The maximum number of bins (used only if `breaks=NULL`).
#' @param method `"freq"` for equal frequency binning or `"width"` for equal width binning (used only if `breaks=NULL`).
#'
#' @return A list with the following fields:
#' * `labels`: An integer vector with same length as `x_vec`, where `labels[i]==k` means the i-th element of `x_vec` is in the k-th bin.
#' * `breaks`: Breaks of bins used for binning.
#'
#' @author Kenji Kondo
#'
#' @export
#' @importFrom assertthat assert_that
executeBinning <- function(x_vec, breaks=NULL, nbin.max=100, method="freq") {
  # Check arguments
  assert_that(isBinningFeasible(x_vec))

  # If breaks is NULL, generate bins by self.
  if (is.null(breaks)) {
    left <- min(x_vec)
    right <- max(x_vec)
    if (method == "freq") {
      breaks <- createEqualFreqBins(x_vec, nbin.max)
    } else if (method == "width") {
      breaks <- createEqualWidthBins(left, right, nbin.max)
    } else {
      assert_that(FALSE, msg="wrong 'method' argument.")
    }
  }

  if (is.ordered(x_vec)) {
    labels <- as.integer(rep(1, length(x_vec)))
    for (i in seq(length(breaks))) {
      labels[x_vec >= breaks[i]] <- i + 1
    }
  } else { # integer or numeric cases
    # Calc labels for each element
    labels <- cut(x_vec, breaks=c(-Inf, breaks, Inf), labels=FALSE, right=FALSE)
  }

  return(list(labels=labels, breaks=breaks))
}
