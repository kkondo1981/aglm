#' Fit an AGLM model with cross-validation for \eqn{\lambda}
#'
#' A fitting function with given \eqn{\alpha} and cross-validation for \eqn{\lambda}.
#' See \link{aglm-package} for more details on \eqn{\alpha} and \eqn{\lambda}.
#'
#' @param x
#'   A design matrix.
#'   See \link{aglm} for more details.
#'
#' @param y
#'   A response variable.
#'
#' @param qualitative_vars_UD_only
#'   Same as in \link{aglm}.
#'
#' @param qualitative_vars_both
#'   Same as in \link{aglm}.
#'
#' @param qualitative_vars_OD_only
#'   Same as in \link{aglm}.
#'
#' @param quantitative_vars
#'   Same as in \link{aglm}.
#'
#' @param use_LVar
#'   Same as in \link{aglm}.
#'
#' @param extrapolation
#'   Same as in \link{aglm}.
#'
#' @param add_linear_columns
#'   Same as in \link{aglm}.
#'
#' @param add_OD_columns_of_qualitatives
#'   Same as in \link{aglm}.
#'
#' @param add_interaction_columns
#'   Same as in \link{aglm}.
#'
#' @param OD_type_of_quantitatives
#'   Same as in \link{aglm}.
#'
#' @param nbin.max
#'   Same as in \link{aglm}.
#'
#' @param bins_list
#'   Same as in \link{aglm}.
#'
#' @param bins_names
#'   Same as in \link{aglm}.
#'
#' @param family
#'   Same as in \link{aglm}.
#'
#' @param keep
#'   Set to `TRUE` if you need the `fit.preval` field in the returned value, as in `cv.glmnet()`.
#'
#' @param ...
#'   Other arguments are passed directly when calling `cv.glmnet()`.
#'
#' @return
#'   A model object fitted to the data with cross-validation results.
#'   Functions such as `predict` and `plot` can be applied to the returned object, same as the result of `aglm()`.
#'   See \link{AccurateGLM-class} for more details.
#'
#'
#' @example examples/cv-aglm-1.R
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
#' @importFrom glmnet cv.glmnet
#' @importFrom methods new
cv.aglm <- function(x, y,
                    qualitative_vars_UD_only=NULL,
                    qualitative_vars_both=NULL,
                    qualitative_vars_OD_only=NULL,
                    quantitative_vars=NULL,
                    use_LVar=FALSE,
                    extrapolation="default",
                    add_linear_columns=TRUE,
                    add_OD_columns_of_qualitatives=TRUE,
                    add_interaction_columns=FALSE,
                    OD_type_of_quantitatives="C",
                    nbin.max=NULL,
                    bins_list=NULL,
                    bins_names=NULL,
                    family=c("gaussian","binomial","poisson", "multinomial"),
                    keep=FALSE,
                    ...) {
  # Create an input object
  x <- newInput(x,
                qualitative_vars_UD_only=qualitative_vars_UD_only,
                qualitative_vars_both=qualitative_vars_both,
                qualitative_vars_OD_only=qualitative_vars_OD_only,
                quantitative_vars=quantitative_vars,
                use_LVar=use_LVar,
                extrapolation=extrapolation,
                add_linear_columns=add_linear_columns,
                add_OD_columns_of_qualitatives=add_OD_columns_of_qualitatives,
                add_interaction_columns=add_interaction_columns,
                OD_type_of_quantitatives=OD_type_of_quantitatives,
                nbin.max,
                bins_list,
                bins_names)

  # Check family
  if (is.character(family))
    family <- match.arg(family)

  # Check y
  if (family %in% c("binomial", "multinomial")) {
    if (any(c("data.frame", "matrix") %in% class(y))) {
      if (dim(y)[2] == 1)
        y <- y[, 1]
      else
        y <- as.matrix(y)
    }

    if ("matrix" %in% class(y)) {
      nc <- dim(y)[2]
    } else {
      if (!is.factor(y))
        y <- factor(y)
      nc <- length(levels(y))
    }
    assert_that(family != "binomial" || nc == 2)
    assert_that(family != "multinomial" || nc > 2)
  } else {
    y <- drop(y)
    y <- as.numeric(y)
  }

  # Create a design matrix which is passed to backend API
  x_for_backend <- getDesignMatrix(x)

  # Data size
  nobs <- dim(x_for_backend)[1]
  nvars <- dim(x_for_backend)[2]
  if ("matrix" %in% class(y))
    assert_that(dim(y)[1] == nobs)
  else
    assert_that(length(y) == nobs)

  # Call backend
  args <- list(x=x_for_backend,
               y=y,
               family=family,
               keep=keep,
               ...)

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
             nzero=as.integer(cv.glmnet_result$nzero),
             name=cv.glmnet_result$name,
             lambda.min=cv.glmnet_result$lambda.min,
             lambda.1se=cv.glmnet_result$lambda.1se,
             fit.preval=as.matrix(cv.glmnet_result$fit.preval),
             foldid=cv.glmnet_result$foldid,
             vars_info=x@vars_info,
             call=match.call()))
}
