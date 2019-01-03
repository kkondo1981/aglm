
#' @export
coef.AccurateGLM <- function(fitted, s=NULL, exact=FALSE, ...) {
  return(coef(fitted@backend_models[[1]], s, exact, ...))
}
