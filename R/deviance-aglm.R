#' Get deviance
#'
#' @param object
#'   A model object obtained from `aglm()` or `cv.aglm()`.
#'
#' @param s
#'   A numeric value specifying \eqn{\lambda} at which deviance is calculated.
#'
#' @param ...
#'   Other arguments are passed directly to `deviance.glmnet()`.
#'
#' @return
#'   The value of deviance extracted from the object `object`.
#'
#'
#' @author
#' Kenji Kondo
#'
#'
#' @export
#' @importFrom stats deviance
deviance.AccurateGLM <- function(object, s=NULL, ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- object

  dev <- deviance(model@backend_models[[1]], ...)
  if (!is.null(s)) {
    dev <- dev[match(s, model@lambda)]
  }

  return(dev)
}
