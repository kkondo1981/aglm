# plotting function for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Plot coefficients from an AccurateGLM object
#'
#' @param model An AccurateGLM object.
#' @param xvar What is on the X-axis.
#'   * `"norm"` plots against the L1-norm of the coefficients.
#'   * `"lambda"` against the log-lambda sequence.
#'   * `"dev"` against the percent deviance explained.
#' @param label A boolean value. If `TRUE`, label the curves with variable sequence numbers.
#' @param ... Other arguments are passed directly to `plot` functions of `model@backend_models`.
#'
#' @export
plot.AccurateGLM <- function(model, xvar=c("norm","lambda","dev"), label=FALSE, ...) {
  plot(fitted@backend_models[[1]], xvar, label, ...)
}
