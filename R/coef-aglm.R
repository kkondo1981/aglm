# calculate deviances for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Extract the deviance from an AccurateGLM object.
#'
#' @param model An AccurateGLM object.
#' @param s  Value(s) of the penalty parameter `lambda` at which predictions are required.
#'   Default is the entire sequence used to create the model.
#' @param ... Other arguments are passed directly to `deviance` functions of `model@backend_models`.
#'
#' @export
coef.AccurateGLM <- function(model, s=NULL, exact=FALSE, ...) {
  return(coef(model@backend_models[[1]], s, exact, ...))
}
