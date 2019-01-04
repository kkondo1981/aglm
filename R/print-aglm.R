# printing function for AGLM
# witten by Kenji Kondo @ 2019/1/3

#' Print an AccurateGLM object
#'
#' @param model An AccurateGLM object.
#' @param digits Significant digits in printout.
#' @param ... Other arguments are passed directly to `print` functions of `model@backend_models`.
#'
#' @export
print.AccurateGLM <- function(model, digits=max(3, getOption("digits") - 3), ...) {
  print(model@backend_models[[1]], digits, ...)
}
