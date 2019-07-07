# calculate residuals for AGLM model
# written by Kenji Kondo @ 2019/7/7

#' Calculate residuals for AGLM model
#'
#' @param model An AccurateGLM object.
#' @param type Type of prediction required.
#'   * Type `"working"` Working residuals.
#' @param s A numeric value specifying lambda value at which plotting is required.
#'
#' @return The object returned depends on type.
#'
#' @export
#' @importFrom assertthat assert_that
residuals.AccurateGLM <- function(model,
                                  x=NULL,
                                  y=NULL,
                                  type=c("working"),
                                  s=NULL) {
  # Check and set `type`
  type <- match.arg(type)

  # Get x and y from model@call
  call.orig <- getCall(model)
  if (is.null(x)) {
    x <- eval(call.orig$x)
    if (class(x) != "data.frame") x <- data.frame(x)
  }
  if (is.null(y)) {
    y <- as.numeric(drop(eval(call.orig$y)))
  }
  assert_that(dim(x)[1] == length(y))

  # Calculate residuals
  yhat <- as.numeric(drop(predict(model, newx=x, s=s, type="response")))
  resids <- y - yhat
  cl <- class(model@backend_models[[1]])
  if ("fishnet" %in% cl) resids <- resids / yhat
  else if ("lognet" %in% cl) resids <- resids / (yhat * (1 - yhat))

  return(resids)
}
