# calculate deviances for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Extract the deviance from an AccurateGLM object.
#'
#' @param object An AccurateGLM object.
#' @param ... Other arguments are passed directly to `deviance` functions of `model@backend_models`.
#'
#' @export
deviance.AccurateGLM <- function(object, ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- object

  return(deviance(model@backend_models[[1]], ...))
}
