# calculate residuals for AGLM model
# written by Kenji Kondo @ 2019/7/7

#' Calculate residuals for AGLM model
#'
#' @param object An `AccurateGLM` object.
#' @param x An input matrix or data.frame used for predictions in residual calculations.
#'   If not given, `x` used for fitting the model is used.
#' @param y A numeric vector used as true target values in residual calculations.
#'   If not given, `y` used for fitting the model is used.
#' @param offset A numeric offset values used for predictions in residual calculations.
#'   If not given, `offset` used for fitting the model is used.
#' @param weights A numeric weight values, corresponding with exposure size.
#'   If not given, `weights` used for fitting the model is used.
#' @param type Type of prediction required.
#'   * Type `"working"` Working residuals.
#'   * Type `"pearson"` Pearson residuals.
#'   * Type `"deviance"` Deviance residuals.
#' @param s A numeric value specifying lambda value at which plotting is required.
#' @param ... Other arguments are currently not used.
#'
#' @return The object returned depends on type.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom stats predict
#' @importFrom stats getCall
residuals.AccurateGLM <- function(object,
                                  x=NULL,
                                  y=NULL,
                                  offset=NULL,
                                  weights=NULL,
                                  type=c("working", "pearson", "deviance"),
                                  s=NULL,
                                  ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- object

  # Check and set `type`
  type <- match.arg(type)

  # Get x and y from model@call
  call.orig <- getCall(model)
  if (is.null(x)) {
    x <- eval.parent(call.orig$x)
    if (class(x)[1] != "data.frame") x <- data.frame(x)
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
  if (class(x)[1] != "data.frame") x <- data.frame(x)
  assert_that(dim(x)[1] == length(y))
  assert_that(length(y) == length(weights))

  # Calculate residuals
  cl <- class(model@backend_models[[1]])

  if (type == "working") {
    yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
    resids <- sqrt(weights) * (y - yhat)
    if ("fishnet" %in% cl)
      resids <- resids / yhat  # Poisson case
    else if ("lognet" %in% cl)
      resids <- resids / (yhat * (1 - yhat))  # binomial case
  } else if (type == "pearson") {
    yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
    resids <- sqrt(weights) * (y - yhat)
    if ("fishnet" %in% cl)
      resids <- resids / sqrt(yhat) # Poisson case
    else if ("lognet" %in% cl)
      resids <- resids / sqrt(yhat * (1 - yhat)) # binomial case
  } else if (type == "deviance") {
    if ("fishnet" %in% cl){  # Poisson case
      yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
      z <- 2 * (log((y / yhat)^y) - y + yhat)
      resids <- sqrt(weights) * sign(y - yhat) * sqrt(abs(z))
    } else if ("lognet" %in% cl) {  # binomial case
      eta <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="link")))
      z <- 2 * (log(1 + exp(eta) - y * eta))
      resids <- sqrt(weights) * sign(z) * sqrt(abs(z))
    } else {  # Gaussian case
      yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
      resids <- sqrt(weights) * (y - yhat)
    }
  } else {
    assert_that(FALSE)  # never comes here
  }

  return(resids)
}
