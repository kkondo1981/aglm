
#' Plot coefficients from an AccurateGLM object
#'
#' @export
plot.AccurateGLM <- function(fitted, xvar=c("norm","lambda","dev"), label=FALSE, ...) {
  plot(fitted@backend_models[[1]], xvar, label, ...)
}
