#' Fit an AGLM model with cross-validation for both \eqn{\alpha} and \eqn{\lambda}
#'
#' A fitting function with cross-validation for both \eqn{\alpha} and \eqn{\lambda}.
#' See \link{aglm-package} for more details on \eqn{\alpha} and \eqn{\lambda}.
#'
#' @param x
#'   A design matrix.
#'   See \link{aglm} for more details.
#'
#' @param y
#'   A response variable.
#'
#' @param alpha
#'   A numeric vector representing \eqn{\alpha} values to be examined in cross-validation.
#'
#' @param nfolds
#'   An integer value representing the number of folds.
#'
#' @param foldid
#'   An integer vector with the same length as observations.
#'   Each element should take a value from 1 to `nfolds`, identifying which fold it belongs.
#'
#' @param parallel.alpha
#'   (not used yet)
#'
#' @param ...
#'   Other arguments are passed directly to `cv.aglm()`.
#'
#' @return
#'   An object storing fitted models and information of cross-validation.
#'   See \link{CVA_AccurateGLM-class} for more details.
#'
#'
#' @example examples/cva-aglm-1.R
#'
#'
#' @author
#'   * Kenji Kondo,
#'   * Kazuhisa Takahashi and Hikari Banno (worked on L-Variable related features)
#'
#'
#' @references Suguru Fujita, Toyoto Tanaka, Kenji Kondo and Hirokazu Iwasawa. (2020)
#' \emph{AGLM: A Hybrid Modeling Method of GLM and Data Science Techniques}, \cr
#' \url{https://www.institutdesactuaires.com/global/gene/link.php?doc_id=16273&fg=1} \cr
#' \emph{Actuarial Colloquium Paris 2020}
#'
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom methods new
cva.aglm <- function(x, y,
                     alpha=seq(0, 1, len=11)^3,
                     nfolds=10,
                     foldid=NULL,
                     parallel.alpha=FALSE,
                     ...) {
  nfolds <- as.integer(nfolds)

  ## The function called to search lambda
  .cvfunc <- function(a, x, y, nfolds, foldid, ...) {
    cv.aglm(x, y, alpha=a, nfolds=nfolds, foldid=foldid, ...)
  }

  ## Calculates for all alphas and lambdas
  if (parallel.alpha) {
    assert_that(FALSE, msg="parallel computation is not implemented yet.")
  } else {
    modlist <- lapply(alpha, .cvfunc, x=x, y=y, nfolds=nfolds, foldid=foldid, ...)
  }

  ## Finds the pair (alpha, lambda), which achieves minimum loss
  alpha.min.index <- which.min(lapply(modlist, function(mod){min(mod@cvm)}))
  if (length(alpha.min.index) > 1) alpha.min.index <-alpha.min.index[1]
  alpha.min <-alpha[alpha.min.index]
  lambda.min <- modlist[[alpha.min.index]]@lambda.min

  return(new("CVA_AccurateGLM", models_list=modlist,
             alpha=alpha,
             nfolds=nfolds,
             alpha.min.index=alpha.min.index,
             alpha.min=alpha.min,
             lambda.min=lambda.min,
             call=match.call))
}
