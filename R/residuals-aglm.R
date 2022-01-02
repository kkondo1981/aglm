#' Get residuals of various types
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
#'   \loadmathjax
#'   A string representing type of deviance:
#'   * `"working"` get working residual
#'      \mjsdeqn{r^W_i = (y_i - \mu_i) \left(\frac{\partial \eta}{\partial \mu}\right)_{\mu=\mu_i},}
#'      where \eqn{y_i} is a response value, \eqn{\mu} is GLM mean, and \eqn{\eta=g(\mu)} is a value of the link function \eqn{g}.
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

  # Get call
  call.orig <- getCall(model)

  # Get family
  family0 <- call.orig$family
  if (is.null(family0)) family0 <- "gaussian"
  if (is.character(family0)) {
    if (family0 == "gaussian") family <- gaussian(link="identity")
    else if (family0 == "binomial") family <- binomial(link="logit")
    else if (family0 == "poisson") family <- poisson(link="log")
    else assert_that(FALSE)  # not implemented yet
  } else {  # general case
    family0 <- ""
    family <- model@backend_models[[1]]$family
  }

  # Get x and y from call
  if (is.null(x)) {
    x <- eval.parent(call.orig$x)
    if (class(x)[1] != "data.frame") x <- data.frame(x)
  }

  if (is.null(y))
    y <- eval.parent(call.orig$y)

  if (family0 %in% c("binomial", "multinomial")) {
    if (any(c("data.frame", "matrix") %in% class(y))) {
      if (dim(y)[2] == 1)
        y <- y[, 1]
      else
        y <- as.matrix(y)
    }
  } else {
    y <- drop(y)
    y <- as.numeric(y)
  }

  y_tot <- NULL
  if (family0 == "binomial") {
    if ("matrix" %in% class(y)) {
      y_tot <- rowSums(y)
      y_hit <- y[, 2]
      y <- y_hit / y_tot
    } else {
      if (!is.factor(y))
        y <- factor(y)
      y <- as.integer(y == levels(y)[2])
    }
  } else if (family0 == "multinomial") {
    assert_that(FALSE)  # not implemented yet
  }

  # Get offset and weights from call
  if (!is.null(call.orig$offset) & is.null(offset)) {
    offset <- as.numeric(drop(eval.parent(call.orig$offset)))
  }
  if (is.null(weights)) {
    weights <- as.numeric(drop(eval.parent(call.orig$weights)))
    if (is.null(weights) || length(weights) == 0) weights <- rep(1, length(y))
    if (!is.null(y_tot)) weights <- weights * y_tot ** 2
  }

  # Check variables
  if (class(x)[1] != "data.frame") x <- data.frame(x)
  assert_that(dim(x)[1] == length(y))
  if (!is.null(offset)) assert_that(length(y) == length(offset))
  assert_that(length(y) == length(weights))

  # Calculate residuals
  if (type == "working") {
    yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
    resids <- sqrt(weights) * (y - yhat) / family$mu.eta(family$linkfun(yhat))
  } else if (type == "pearson") {
    yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
    resids <- sqrt(weights) * (y - yhat) / sqrt(family$variance(yhat))
  } else if (type == "deviance") {
    if ("fishnet" %in% cl){  # Poisson case
      yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
      z <- 2 * (log((y / yhat)^y) - y + yhat)
      resids <- sqrt(weights) * sign(y - yhat) * sqrt(abs(z))
    } else if ("lognet" %in% cl) {  # binomial case
      eta <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="link")))
      z <- 2 * (log(1 + exp(eta) - y * eta))
      resids <- sqrt(weights) * sign(z) * sqrt(abs(z))
    } else if ("glmnetfit" %in% cl) {  # general case
      assert_that(FALSE)  # not implemented yet
    } else {  # Gaussian case
      yhat <- as.numeric(drop(predict(model, newx=x, newoffset=offset, s=s, type="response")))
      resids <- sqrt(weights) * (y - yhat)
    }
  } else {
    assert_that(FALSE)  # never comes here
  }

  return(resids)
}
