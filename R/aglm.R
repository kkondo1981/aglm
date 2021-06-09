#' Fit an AGLM model with no cross-validation
#'
#' A basic fitting function with given \eqn{\alpha} and \eqn{\lambda} (s).
#' See \link{aglm-package} for more details on \eqn{\alpha} and \eqn{\lambda}.
#'
#' @param x
#'   A design matrix.
#'   Usually a `data.frame` object is expected, but a `matrix` object is fine if all columns are of a same class.
#'   Each column may have one of the following classes, and `aglm` will automatically determine how to handle it:
#'   * `numeric`: interpreted as a quantitative variable. `aglm` performs discretization by binning, and creates dummy variables suitable for ordered values (named O-dummies/L-variables).
#'   * `factor` (unordered) or `logical` : interpreted as a qualitative variable without order. `aglm` creates dummy variables suitable for unordered values (named U-dummies).
#'   * `ordered`: interpreted as a qualitative variable with order. `aglm` creates both O-dummies and U-dummies.
#'
#'   These dummy variables are added to `x` and form a larger matrix, which is used internally as an actual design matrix.
#'   See \href{https://www.institutdesactuaires.com/global/gene/link.php?doc_id=16273&fg=1}{our paper} for more details on O-dummies, U-dummies, and L-variables.
#'
#'   If you need to change the default behavior, use the following options: `qualitative_vars_UD_only`, `qualitative_vars_both`, `qualitative_vars_OD_only`, and `quantitative_vars`.
#'
#' @param y
#'   A response variable.
#'
#' @param qualitative_vars_UD_only
#'   Used to change the default behavior of `aglm` for given variables.
#'   Variables specified by this parameter are considered as qualitative variables and only U-dummies are created as auxiliary columns.
#'   This parameter may have one of the following classes:
#'   * `integer`: specifying variables by index.
#'   * `character`: specifying variables by name.
#'
#' @param qualitative_vars_both
#'   Same as `qualitative_vars_UD_only`, except that both O-dummies and U-dummies are created for specified variables.
#'
#' @param qualitative_vars_OD_only
#'   Same as `qualitative_vars_UD_only`, except that both only O-dummies are created for specified variables.
#'
#' @param quantitative_vars
#'   Same as `qualitative_vars_UD_only`, except that specified variables are considered as quantitative variables.
#'
#' @param use_LVar
#'   Set to use L-variables.
#'   By default, `aglm` uses O-dummies as the representation of a quantitative variable.
#'   If `use_LVar=TRUE`, L-variables are used instead.
#'
#' @param extrapolation
#'   Used to control values of linear combination for quantitative variables, outside where the data exists.
#'   By default, values of a linear combination outside the data is extended based on the slope of the edges of the region where the data exists.
#'   You can set `extrapolation="flat"` to get constant values outside the data instead.
#'
#' @param add_linear_columns
#'   By default, for quantitative variables, `aglm` expands them by adding dummies and the original columns, i.e. the linear effects, are remained in the resulting model.
#'   You can set `add_linear_columns=FALSE` to drop linear effects.
#'
#' @param add_OD_columns_of_qualitatives
#'   Set to `FALSE` if you do not want to use O-dummies for qualitative variables with order (usually, columns with `ordered` class).
#'
#' @param add_interaction_columns
#'   If this parameter is set to `TRUE`, `aglm` creates an additional auxiliary variable `x_i * x_j` for each pair `(x_i, x_j)` of variables.
#'
#' @param OD_type_of_quantitatives
#'   Used to control the shape of linear combinations obtained by O-dummies for quantitative variables (deprecated).
#'
#' @param family
#'   A `family` object or a string representing the type of the error distribution.
#'   Currently `aglm` supports `gaussian`, `binomial`, and `poisson`.
#'
#' @param nbin.max
#'   An integer representing the maximum number of bins when `aglm` perform binning for quantitative variables.
#'
#' @param bins_list
#'   Used to set custom bins for variables with O-dummies.
#'
#' @param bins_names
#'   Used to set custom bins for variables with O-dummies.
#'
#' @param ...
#'   Other arguments are passed directly when calling `glmnet()`.
#'
#' @return
#'   A model object fitted to the data.
#'   Functions such as `predict` and `plot` can be applied to the returned object.
#'   See \link{AccurateGLM-class} for more details.
#'
#'
#' @example examples/aglm-1.R
#' @example examples/aglm-2.R
#' @example examples/lvar-and-extrapolation.R
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
#' @importFrom glmnet glmnet
#' @importFrom methods new
aglm <- function(x, y,
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
                 family=c("gaussian","binomial","poisson"),
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

  # Check y
  y <- drop(y)
  y <- as.numeric(y)
  #assert_that(class(y) == "integer" | class(y) == "numeric")
  assert_that(length(y) == dim(x@data)[1])

  # Check family
  if (is.character(family))
    family <- match.arg(family)

  # Create a design matrix which is passed to backend API
  x_for_backend <- getDesignMatrix(x)

  # Data size
  nobs <- dim(x_for_backend)[1]
  nvars <- dim(x_for_backend)[2]
  assert_that(length(y) == nobs)

  # Call backend
  args <- list(x=x_for_backend,
               y=y,
               family=family,
               ...)
  glmnet_result <- do.call(glmnet, args)

  return(new("AccurateGLM", backend_models=list(glmnet=glmnet_result), vars_info=x@vars_info, call=match.call()))
}
