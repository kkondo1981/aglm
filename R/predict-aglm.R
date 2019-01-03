# predicting function for AGLM model
# written by Kenji Kondo @ 2019/1/3

#' Predicts response variables for new explanatory variables.
#'
#' @param fitted An AccurateGLM object fitted to data by aglm() function.
#' @param newx,newx_UD,UD_vars New data or predictor variables corresponding to response variables
#'   to be predicted. See descriptions of aglm() for more details of usages.
#' @param standardize_quantitative_vars same as aglm()'s `standardize_quantitative_vars`.
#' @param ... Other arguments are passed directly to backend (currently glmnet() is used), and
#'   if not given, backend API's default values are used to call backend functions.
#'
#' @return same as glmnet()
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom glmnet predict.glmnet
predict.AccurateGLM <- function(fitted,
                                newx, newx_UD=NULL, UD_vars=NULL,
                                standardize_quantitative_vars=TRUE,
                                s=NULL,
                                type=c("link","response","coefficients","nonzero","class"),
                                exact=FALSE,
                                newoffset,
                                ...) {
  # Create a PredVars object if not given
  if (is.null(newx) | class(newx) != "PredVars") newx <- newPredVars(newx, newx_UD, UD_vars)

  # Use same vars_info to be compatible with the fitted model
  newx@vars_info <- fitted@vars_info

  # Create a design matrix which is passed to backend API
  x_for_backend <- getDesignMatrix(newx, standardize_quantitative_vars=standardize_quantitative_vars)

  # Data size
  # nobs <- dim(x_for_backend)[1]
  # nvars <- dim(x_for_backend)[2]

  glmnet_result <- predict(fitted@backend_models[[1]],
                           x_for_backend,
                           s=s,
                           type=type,
                           exact=exact,
                           ...)

  return(glmnet_result)
}
