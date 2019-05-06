# cross-validation function for both alpha and lambda
# written by Kenji Kondo @ 2019/5/6


#' Cross-validation for both alpha and lambda
#'
#' @param x An input matrix or data.frame to be fitted.
#' @param y An integer or numeric vector which represents response variable.
#' @param alpha A numeric vector which represents alpha values to be examined.
#' @param nfolds The number of folds. The default value is 10.
#' @param foldid An integer vector of values between 1 and nfold identifying what fold each observation is in.
#' @param parallel.alpha not implemented yet.
#' @param ... Other arguments are passed directly to cv.aglm().
#'
#' @return Result of cross-validation.
#'
#' @export
#' @importFrom assertthat assert_that
cva.aglm <- function(x, y,
                     alpha=seq(0, 1, len=11)^3,
                     nfolds=10,
                     foldid=sample(rep(seq(nfolds), length=nrow(x))),
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
             lambda.min=lambda.min))
}
