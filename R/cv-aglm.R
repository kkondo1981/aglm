# cross-validation function for AGLM model
# written by Kenji Kondo @ 2019/2/24


#' Cross-validation for AGLM models
#'
#' @param x An input matrix or data.frame to be fitted.
#' @param y An integer or numeric vector which represents response variable.
#' @param qualitative_vars_UD_only A list of indices or names for specifying which columns are qualitative and need only U-dummy representations.
#' @param qualitative_vars_both A list of indices or names for specifying which columns are qualitative and need both U-dummy and O-dummy representations.
#' @param qualitative_vars_OD_only A list of indices or names for specifying which columns are qualitative and need only O-dummy representations.
#' @param quantitative_vars A list of indices or names for specyfying which columns are quantitative.
#' @param add_linear_columns A boolean value which indicates whether this function uses linear effects or not.
#' @param add_OD_columns_of_qualitatives A boolean value which indicates whether this function use O-dummy representations for qualitative and ordinal variables or not.
#' @param add_interaction_columns A boolean value which indicates whether this function uses interaction effects or not.
#' @param OD_type_of_quantitatives A character value which indicates how O-dummy matrices of quantitative
#'   values are constructed. Choose 'C'(default) or 'J'.
#'   * 'C': Continuous-type dummies, which result continuous contribution curves.
#'   * 'J': Jum-type dummies, which result contribution curves with jumps.
#' @param family Response type. Currently "gaussian", "binomial", and "poisson" are supported.
#' @param bins_list A list of numeric vectors, each element of which is used as breaks when binning of a quantitative variable or a qualitative variable with order.
#' @param bins_names A list of column name or column index, each name or index of which specifies which column of `x` is binned used with an element of `bins_list` in the same position.
#' @param ... Other arguments are passed directly to backend (currently cv.glmnet() is used), and if not given, backend API's default values are used to call backend functions.
#'
#' @return Result of cross-validation.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom glmnet cv.glmnet
cv.aglm <- function(x, y,
                    qualitative_vars_UD_only=NULL,
                    qualitative_vars_both=NULL,
                    qualitative_vars_OD_only=NULL,
                    quantitative_vars=NULL,
                    add_linear_columns=TRUE,
                    add_OD_columns_of_qualitatives=TRUE,
                    add_interaction_columns=TRUE,
                    OD_type_of_quantitatives='C',
                    bins_list=NULL,
                    bins_names=NULL,
                    weights,
                    offset=NULL,
                    lambda=NULL,
                    type.measure,
                    nfolds = 10,
                    foldid,
                    grouped = TRUE,
                    keep = FALSE,
                    parallel = FALSE,
                    standardize=TRUE,
                    family=c("gaussian","binomial","poisson"),
                    alpha=1.0,
                    nlambda=100,
                    lambda.min.ratio=NULL,
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
  # Create an input object
  x <- newInput(x,
                qualitative_vars_UD_only=qualitative_vars_UD_only,
                qualitative_vars_both=qualitative_vars_both,
                qualitative_vars_OD_only=qualitative_vars_OD_only,
                quantitative_vars=quantitative_vars,
                add_linear_columns=add_linear_columns,
                add_OD_columns_of_qualitatives=add_OD_columns_of_qualitatives,
                add_interaction_columns=add_interaction_columns,
                OD_type_of_quantitatives=OD_type_of_quantitatives,
                bins_list,
                bins_names)

  # Check y
  y <- drop(y)
  #assert_that(class(y) == "integer" | class(y) == "numeric")
  y <- as.numeric(y)
  assert_that(length(y) == dim(x@data)[1])

  # Check family
  family <- match.arg(family)

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


  args <- list(x=x_for_backend,
               y=y,
               nfolds=nfolds,
               grouped=grouped,
               keep=keep,
               parallel=parallel,
               family=family,
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
               penalty.factor=penalty.factor,
               lower.limits=lower.limits,
               upper.limits=upper.limits,
               maxit=maxit,
               type.gaussian=type.gaussian,
               type.logistic=type.logistic,
               standardize.response=standardize.response)

  if(!missing(type.measure)) args$type.measure <- type.measure
  if(!missing(weights)) args$weights <- weights
  if(!missing(foldid)) args$foldid <- foldid
  if(!missing(exclude)) args$exclude <- exclude

  cv.glmnet_result <- do.call(cv.glmnet, args)

  if (!keep) {
    cv.glmnet_result$fit.preval <- matrix(0)
    cv.glmnet_result$foldid <- integer(0)
  }

  return(new("AccurateGLM", backend_models=list(cv.glmnet=cv.glmnet_result$glmnet.fit),
             lambda=cv.glmnet_result$lambda,
             cvm=cv.glmnet_result$cvm,
             cvsd=cv.glmnet_result$cvsd,
             cvup=cv.glmnet_result$cvup,
             cvlo=cv.glmnet_result$cvlo,
             nzero=cv.glmnet_result$nzero,
             name=cv.glmnet_result$name,
             lambda.min=cv.glmnet_result$lambda.min,
             lambda.1se=cv.glmnet_result$lambda.1se,
             fit.preval=cv.glmnet_result$fit.preval,
             foldid=cv.glmnet_result$foldid,
             vars_info=x@vars_info,
             call=match.call()))
}
