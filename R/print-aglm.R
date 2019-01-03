
#' Print an AccurateGLM object
#'
#' @export
print.AccurateGLM <- function(fitted, digits=max(3, getOption("digits") - 3), ...) {
  print(fitted@backend_models[[1]], digits, ...)
}
