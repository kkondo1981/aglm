# calculate residuals for AGLM model
# written by Kenji Kondo @ 2019/7/7

#' Calculate residuals for AGLM model
#'
#' @param model An AccurateGLM object.
#' @param x,y Data of which residuals are calculated.
#' @param type Type of prediction required.
#'   * Type `"working"` Working residuals.
#' @param s A numeric value specifying lambda value at which plotting is required.
#'
#' @return The object returned depends on type.
#'
#' @export
#' @importFrom assertthat assert_that
residuals.AccurateGLM <- function(model,
                                  x, y,
                                  type=c("working"),
                                  s=NULL) {
  # Check and set `type`
  type <- match.arg(type)

  yhat <- predict(model, newx=x, s=s, type="response")
  resids <- y - yhat
  cl <- class(model@backend_models[[1]])
  if ("fishnet" %in% cl) resids <- resids / yhat
  else if ("lognet" %in% cl) resids <- resids / (yhat * (1 - yhat))

  return(resids)
}
