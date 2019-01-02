# fitting function for AGLM model
# written by Kenji Kondo @ 2019/1/1


#' fit an AGLM model
#'
#' @param x Explanatory or predictor variables used for fitting AGLM.
#'   Although this function expects `x` would a PredVars object, it also can be matrix or data.frame.
#'   Is such cases, this function automatically convert it to a PredVars object by calling
#'   `newPredVars(x, x_UD, UD_vars)` and use that PredVars object as it is initially given.
#'   Furthermore, `x` could be NULL in case where `x_UD` is given.
#'   See descriptions of `newPredVars()` for details of non-PredVars cases.
#' @param y An integer or numeric vector which represents response variable.
#' @param x_UD,UD_vars See descriptions of `x` for details. Note that these values are required
#'   only if a non-PredVar value are given as `x`.
#' @param family Response type. Currently "gaussian", "binomial", and "poisson" are supported.
#' @param standardize A boolean value which indicates whether standardizes each column of `x`.
#'   Note that columns of `x_UD` and columns of `x` specified by `UD_vars` are not standardized
#'   even if `standardize=TRUE`, because this function considers these columns as quantitative.
#' @param ... Other arguments are passed directly to backend API (currently glmnet() is used),
#'   and if not given, backend API's default values are used.
#'
#' @return An AccurateGLM object, fitted to the data (x, y)
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom glmnet glmnet
aglm <- function(x, y, x_UD=NULL,UD_vars=NULL,
                 family=c("gaussian","binomial","poisson"),
                 weights,
                 offset=NULL,
                 alpha=1.0,
                 nlambda=100,
                 lambda.min.ratio=NULL,
                 lambda=NULL,
                 standardize=TRUE,
                 intercept=TRUE,
                 thresh=1e-7,
                 dfmax=NULL,
                 pmax=NULL,
                 exclude,
                 penalty.factor=NULL,
                 lower.limits=-Inf,
                 upper.limits=Inf,
                 maxit=100000,
                 type.gaussian=NULL,
                 type.logistic=c("Newton","modified.Newton"),
                 standardize.response=FALSE) {
  # Create a PredVars object if not given
  if (is.null(x) | class(x) != "PredVars") x <- newPredVars(x, x_UD, UD_vars)

  # Check y
  y <- drop(y)
  assert_that(class(y) == "integer" | class(y) == "numeric")
  assert_that(length(y) == dim(x@data)[1])

  # Create a design matrix which is passed to backend API
  x_for_backend <- getDesignMatrix(x)

  # Data size
  nobs <- dim(x_for_backend)[1]
  nvars <- dim(x_for_backend)[2]
  assert_that(length(y) == nobs)

  # Set default values to some parameters if not given
  if (is.null(lambda.min.ratio)) lambda.min.ratio <- ifelse(nobs<nvars,1e-2,1e-4)
  if (is.null(dfmax)) dfmax <- nvars+1
  if (is.null(pmax)) pmax <- min(dfmax*2+20,nvars)
  if (is.null(penalty.factor)) penalty.factor <- rep(1,nvars)
  if (is.null(type.gaussian)) type.gaussian <- ifelse(nvars<500,"covariance","naive")

  glmnet_result <- glmnet(x=x_for_backend,
                          y=y,
                          family=family,
                          weights=weights,
                          offset=offset,
                          alpha=alpha,
                          nlambda=nlambda,
                          lambda.min.ratio=lambda.min.ratio,
                          lambda=lambda,
                          standardize=standardize,
                          intercept=intercept,
                          thresh=thresh,
                          dfmax=dfmax,
                          pmax=pmax,
                          exclude=exclude,
                          penalty.factor=penalty.factor,
                          lower.limits=lower.limits,
                          upper.limits=upper.limits,
                          maxit=maxit,
                          type.gaussian=type.gaussian,
                          type.logistic=type.logistic,
                          standardize.response=standardize.response)

  return(new("AccurateGLM", backend_models=list(glmnet=glmnet_result), vars_info=x@vars_info))
}
