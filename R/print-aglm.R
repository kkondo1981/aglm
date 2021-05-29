# printing function for AGLM
# witten by Kenji Kondo @ 2019/1/3

#' Print an `AccurateGLM` object
#'
#' @param x An `AccurateGLM` object.
#' @param digits Significant digits in printout.
#' @param ... Other arguments are passed directly to `print` functions of `model@backend_models`.
#'
#' @export
print.AccurateGLM <- function(x, digits=max(3, getOption("digits") - 3), ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- x

  print(model@backend_models[[1]], digits, ...)
}
