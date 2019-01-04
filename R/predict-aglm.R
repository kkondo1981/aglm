# predicting function for AGLM model
# written by Kenji Kondo @ 2019/1/3

#' Make predictions from a fitted AccurateGLM
#'
#' @param model An AccurateGLM object.
#' @param newx New values for `x_UD` at which predictions are to be made.
#'   Although this function expects `newx` would be a `PredVars` object, it also can be matrix or data.frame.
#'   In such cases, this function automatically convert it to a PredVars object by calling `newPredVars(newx, newx_UD, UD_vars)`.
#'   See the descriptions of `newPredVars` function for more details.
#' @param newx_UD,UD_vars See the descriptions of `newx`. Note that these values are required if and only if `newx` is not a `PredVars` object.
#' @param s  Value(s) of the penalty parameter `lambda` at which predictions are required.
#'   Default is the entire sequence used to create the model.
#' @param type Type of prediction required.
#'   * Type `"link"` gives the linear predictors for `"binomial"`, `"poisson"` models, and for `"gaussian"` models it gives the fitted values.
#'   * Type `"response"` gives the fitted probabilities for `"binomial"`, fitted mean for `"poisson"`, and for `"gaussian"` models it is equivalent to type `"link"`.
#'   * Type `"coefficients"` computes the coefficients at the requested values for `s`.
#'     Note that for `"binomial"` models, results are returned only for the class corresponding to the second level of the factor response.
#'   * Type `"class"` applies only to `"binomial"`, and produces the  class label corresponding to the maximum probability.
#'   * Type `"nonzero"` returns a list of the indices of the nonzero coefficients for each value of `s`.
#' @param ... Other arguments are passed directly to backend (currently glmnet() is used), and if not given, backend API's deault values are used.
#'
#' @return The object returned depends on type.
#'
#' @export
#' @importFrom glmnet predict.glmnet
predict.AccurateGLM <- function(model,
                                newx, newx_UD=NULL, UD_vars=NULL,
                                standardize_quantitative_vars=TRUE,
                                s=NULL,
                                type=c("link","response","coefficients","nonzero","class"),
                                exact=FALSE,
                                newoffset,
                                ...) {
  # Create a PredVars object if not given
  if (is.null(newx) | class(newx) != "PredVars") newx <- newPredVars(newx, newx_UD, UD_vars)

  # Set vars_info of the fitted model for creating a same type of design matrix as in training.
  newx@vars_info <- model@vars_info

  # Create a design matrix passed to backend API
  x_for_backend <- getDesignMatrix(newx)

  glmnet_result <- predict(model@backend_models[[1]],
                           x_for_backend,
                           s=s,
                           type=type,
                           exact=exact,
                           ...)

  return(glmnet_result)
}
