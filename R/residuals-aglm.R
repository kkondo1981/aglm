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
                                  offset=NULL,
                                  weights=NULL,
                                  type=c("working", "pearson"),
                                  s=NULL) {
  # Check and set `type`
  type <- match.arg(type)

  # Get x and y from model@call
  call.orig <- getCall(model)
  if (is.null(x)) {
    x <- eval.parent(call.orig$x)
    if (class(x) != "data.frame") x <- data.frame(x)
  }
  if (is.null(y)) {
    y <- as.numeric(drop(eval.parent(call.orig$y)))
  }
  if (!is.null(call.orig$offset) & is.null(offset)) {
    offset <- as.numeric(drop(eval.parent(call.orig$offset)))
  }
  if (is.null(weights)) {
    weights <- as.numeric(drop(eval.parent(call.orig$weights)))
    if (is.null(weights) || length(weights) == 0) weights <- rep(1, length(y))
  }
  assert_that(dim(x)[1] == length(y))
  assert_that(length(y) == length(weights))

  # Calculate residuals
  yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
  resids <- sqrt(weights) * (y - yhat)

  cl <- class(model@backend_models[[1]])
  if (type == "working") {
    if ("fishnet" %in% cl) resids <- resids / yhat  # Poisson case
    else if ("lognet" %in% cl) resids <- resids / (yhat * (1 - yhat))  # binomial case
  } else if (type == "pearson") {
    if ("fishnet" %in% cl) resids <- resids / sqrt(yhat) # Poisson case
    else if ("lognet" %in% cl) resids <- resids / sqrt(yhat * (1 - yhat)) # binomial case
  } else {
    assert_that(FALSE)
  }

  return(resids)
}
