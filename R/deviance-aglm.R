
#' Extract the deviance from an AccurateGLM object
#'
#' @export
deviance.AccurateGLM <- function(fitted, ...) {
  return(deviance(fitted@backend_models[[1]], ...))
}
