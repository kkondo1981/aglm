#' Make predictions for new data
#'
#' @param object
#'   A model object obtained from `aglm()` or `cv.aglm()`.
#'
#' @param newx
#'   A design matrix for new data.
#'   See the description of `x` in \link{aglm} for more details.
#'
#' @param s
#'   Same as in \link{predict.glmnet}.
#'
#' @param type
#'   Same as in \link{predict.glmnet}.
#'
#' @param exact
#'   Same as in \link{predict.glmnet}.
#'
#' @param newoffset
#'   Same as in \link{predict.glmnet}.

#' @param ...
#'   Other arguments are passed directly when calling `predict.glmnet()`.
#'
#' @return
#'   The returned object depends on `type`.
#'   See \link{predict.glmnet} for more details.
#'
#'
#' @example examples/predict-and-plot-1.R
#'
#'
#' @author
#'   * Kenji Kondo,
#'   * Kazuhisa Takahashi and Hikari Banno (worked on L-Variable related features)
#'
#'
#' @references Suguru Fujita, Toyoto Tanaka, Kenji Kondo and Hirokazu Iwasawa. (2020)
#' \emph{AGLM: A Hybrid Modeling Method of GLM and Data Science Techniques}, \cr
#' \url{https://www.institutdesactuaires.com/global/gene/link.php?doc_id=16273&fg=1} \cr
#' \emph{Actuarial Colloquium Paris 2020}
#'
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom glmnet predict.glmnet
#' @importFrom methods new
#' @importFrom stats predict
predict.AccurateGLM <- function(object,
                                newdata=NULL,
                                s=NULL,
                                type=c("link","response","coefficients","nonzero","class"),
                                exact=FALSE,
                                newoffset=NULL,
                                ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- object

  # Check and set `type`
  type <- match.arg(type)

  if (is.null(object@formula_info)) {
    newx <- newdata
  } else {
    # Create the original design matrix from data and formula
    formula_info <- object@formula_info
    cl <- match.call(expand.dots=FALSE)
    m <- match(c("subset", "na.action", "drop.unused.levels", "xlev"),
               names(cl), 0)
    cl <- cl[c(1, m)]
    cl$formula <- delete.response(formula_info$terms)
    cl$data <- newdata
    cl$offset <- newoffset
    cl$xlev <- formula_info$xlev
    cl[[1]] <- quote(stats::model.frame)
    mf <- eval.parent(cl)

    # The intercept term is not necessary in the original design matrix.
    tf <- attr(mf, "terms")
    attr(tf, "intercept") <- 0

    newx <- model.matrix(tf, mf)
    newoffset <- model.offset(mf)
  }

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
