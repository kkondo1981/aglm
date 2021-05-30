#' Get residuals of various types
#'
#' \loadmathjax
#'
#' @param object
#'   A model object obtained from `aglm()` or `cv.aglm()`.
#'
#' @param x
#'   A design matrix.
#'   If not given, `x` for fitting is used.
#'
#' @param y
#'   A response variable.
#'   If not given, `y` for fitting is used.
#'
#' @param offset
#'   An offset values.
#'   If not given, `offset` for fitting is used.
#'
#' @param weights
#'   Sample weights.
#'   If not given, `weights` for fitting is used.
#'
#' @param type
#'   A string representing type of deviance:
#'   * `"working"` get working residual
#'      \mjsdeqn{r^W_i = (y_i - \mu_i) \left(\frac{\partial \eta}{\partial \mu}\right)_{\mu=\mu_i},}
#'      where \eqn{y_i} is a response value, \eqn{\mu} is GLM mean, and \eqn{\eta=g^{-1}(\mu)} with the link function \eqn{g}.
#'   * `"pearson"` get Pearson residuals
#'      \mjsdeqn{r^P_i = \frac{y_i - \mu_i}{\sqrt{V(\mu_i)}},}
#'      where \eqn{V} is the variance function.
#'   * `"deviance"` get deviance residuals
#'      \mjsdeqn{r^D_i = {\rm sign}(y_i - \mu_i) \sqrt{d_i},}
#'      where \eqn{d_i} is the contribution to deviance.
#'
#' @param s
#'   A numeric value specifying \eqn{\lambda} at which residuals are calculated.
#'
#' @param ...
#'   Other arguments are currently not used and just discarded.
#'
#' @return
#'   A numeric vector representing calculated residuals.
#'
#'
#' @author
#' Kenji Kondo
#'
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
