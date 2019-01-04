# calculate deviances for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Extract the deviance from an AccurateGLM object.
#'
#' @param model An AccurateGLM object.
#' @param ... Other arguments are passed directly to `deviance` functions of `model@backend_models`.
#'
#' @export
deviance.AccurateGLM <- function(model, ...) {
  return(deviance(model@backend_models[[1]], ...))
}
