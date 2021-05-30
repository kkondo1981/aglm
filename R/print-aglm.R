#' Display textual information of the model
#'
#' @param x
#'   A model object obtained from `aglm()` or `cv.aglm()`.
#'
#' @param digits
#'   Used to control significant digits in printout.
#'
#' @param ...
#'   Other arguments are passed directly to `print.glmnet()`.
#'
#'
#' @author
#' Kenji Kondo
#'
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
