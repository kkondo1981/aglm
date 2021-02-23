# predicting function for AGLM model
# written by Kenji Kondo @ 2019/1/3

#' Make predictions from a fitted AccurateGLM
#'
#' @param object An AccurateGLM object.
#' @param newx An input matrix or data.frame used for predictions.
#' @param s Value(s) of the penalty parameter `lambda` at which predictions are required.
#'   Default is the entire sequence used to create the model.
#' @param type Type of prediction required.
#'   * Type `"link"` gives the linear predictors for `"binomial"`, `"poisson"` models, and for `"gaussian"` models it gives the fitted values.
#'   * Type `"response"` gives the fitted probabilities for `"binomial"`, fitted mean for `"poisson"`, and for `"gaussian"` models it is equivalent to type `"link"`.
#'   * Type `"coefficients"` computes the coefficients at the requested values for `s`.
#'     Note that for `"binomial"` models, results are returned only for the class corresponding to the second level of the factor response.
#'   * Type `"class"` applies only to `"binomial"`, and produces the  class label corresponding to the maximum probability.
#'   * Type `"nonzero"` returns a list of the indices of the nonzero coefficients for each value of `s`.
#' @param exact Same as predict.glmnet().
#' @param newoffset If an offset is used in the fit, then one must be supplied for making predictions (except for type="coefficients" or type="nonzero").
#' @param ... Other arguments are passed directly to backend (currently glmnet() is used), and if not given, backend API's deault values are used.
#'
#' @return The object returned depends on type.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom glmnet predict.glmnet
#' @importFrom methods new
#' @importFrom stats predict
predict.AccurateGLM <- function(object,
                                newx=NULL,
                                s=NULL,
                                type=c("link","response","coefficients","nonzero","class"),
                                exact=FALSE,
                                newoffset,
                                ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- object

  # Check and set `type`
  type <- match.arg(type)

  # Create an input object
  if (class(newx)[1] != "data.frame") newx <- data.frame(newx)
  for (i in seq(dim(newx)[2])) {
    var_info <- model@vars_info[[i]]
    if (var_info$type == "quan") newx[, i] <- as.numeric(newx[, i])
    else if (var_info$type == "qual") {
      if (var_info$use_OD & !is.ordered(newx[, i])) newx[, i] <- ordered(newx[, i])
      else if (!is.factor(newx[, i])) newx[, i] <- factor(newx[, i])
    }
  }
  newx <- new("AGLM_Input", vars_info=model@vars_info, data=newx)

  # Create a design matrix passed to backend API
  x_for_backend <- getDesignMatrix(newx)

  # Select what is to be given predict() as a model
  backend_model <- model@backend_models[[1]]

  model_name <- names(model@backend_models)[[1]]
  if (model_name == "cv.glmnet") {
    if (is.character(s)) {
      if (s == "lambda.min")
        s <- model@lambda.min
      if (s == "lambda.1se")
        s <- model@lambda.1se
    }
  }

  glmnet_result <- predict(backend_model,
                           x_for_backend,
                           s=s,
                           type=type,
                           exact=exact,
                           newoffset,
                           ...)

  return(glmnet_result)
}
