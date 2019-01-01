# fitting function for AGLM model
# written by Kenji Kondo @ 2019/1/1


#Inner function used in aglm()
sliceByColWithoutDrop <- function(x, idx) {
  z <- x[, idx]
  if (is.null(dim(z))) {
    z <- matrix(z, dim(x)[1])
    names(z) <- names(x)[idx]
  }
  return(z)
}


#' fit an AGLM model
#'
#' @param x input matrix or data.frame of size (nobs, _). Each row shold be an integer or numeric vector.
#' @param y response variable.
#' @param x_UD additional input matrix of size (nbos, _). optional. Each row of `x_UD` should be an integer,
#'   character, or factor vector and this function replaces it with U-dummy matrix before fitting.
#' @param UD_vars integer or character vector. If an integer vector is set, this function replaces columns whose
#'   indices are in `UD_vars` into U-dummy matrices before fitting. If a character vector is set, this function
#'   replaces each column whose name is in `UD_vars` with U-dummy matrix before fitting.
#'   Note that the columns of `x` are rearanged internally and could affect on outputs of print() and
#'   plot() when `UD_vars` is set. `UD_vars` is ignored when `x_UD` is not NULL.
#'   If both `x_UD` and `UD_vars` are NULL, columns with character types and factor types are automatically
#'   replaced with U-dummy matrices before fitting. The columns of `x` are also rearanged internally in this case.
#' @param family response type. Currently gaussian, binomial, and poisson are supported.
#' @param standardize logical flag for x variable standardization, prior to fitting the model sequence.
#'   Note that aglm() standardizes only x and never x_UD.
#' @param ... other arguments are passed directly to glmnet() and have same default values.
#'
#' @return list with two elements `backend_result` and `vars_info`.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom glmnet glmnet
aglm <- function(x, y, x_UD=NULL,
                 UD_vars=NULL,
                 family=c("gaussian","binomial","poisson"),
                 weights,
                 offset=NULL,
                 alpha=1.0,
                 nlambda=100,
                 #lambda.min.ratio=ifelse(nobs<nvars,1e-2,1e-4),
                 lambda=NULL,
                 standardize=TRUE,
                 intercept=TRUE,
                 thresh=1e-7,
                 #dfmax=nvars+1,
                 #pmax=min(dfmax*2+20,nvars),
                 exclude,
                 #penalty.factor=rep(1,nvars),
                 lower.limits=-Inf,
                 upper.limits=Inf,
                 maxit=100000,
                 #type.gaussian=ifelse(nvars<500,"covariance","naive"),
                 type.logistic=c("Newton","modified.Newton"),
                 standardize.response=FALSE) {
  # Check and process arguments.
  assert_that(class(x) == "matrix" | class(x) == "data.frame")
  if (!is.null(x_UD)) {
    assert_that(class(x_UD) == "matrix" | class(x_UD) == "data.frame")
    assert_that(dim(x)[1] == dim(x_UD)[1])
  }

  y <- drop(y)
  assert_that(class(y) == "integer" | class(y) == "numeric")
  assert_that(length(y) == dim(x)[1])

  nobs <- length(y)


  # Create UD_vars automatically if both UD_vars and x_UD are not given
  if (is.null(UD_vars) & is.null(x_UD)) {
    UD_vars <- (1:dim(x)[2])[(sapply(x, class) == "factor") | (sapply(x, class) == "character")]
    if (length(UD_vars) == 0) UD_vars <- NULL  # For following process
  }


  # Create x_UD from UD_vars if UD_vars is given
  if (!is.null(UD_vars)) {
    assert_that(is.null(x_UD))
    if (is.integer(UD_vars) | is.numeric(UD_vars)) {
      UD_vars <- as.integer(UD_vars)
      x_UD <- sliceByColWithoutDrop(x, UD_vars)
      x <- sliceByColWithoutDrop(x, -UD_vars)
    } else {
      var_names <- colnames(x)
      assert_that(!is.null(var_names))

      x_UD <- sliceByColWithoutDrop(x, var_names %in% UD_vars)
      x <- sliceByColWithoutDrop(x, !(var_names %in% UD_vars))
    }
  }


  # Number of variables
  nvar <- 0
  nvar_UD <- 0
  if (!is.null(x)) nvar <- dim(x)[2]
  if (!is.null(x_UD)) {
    nvar_UD <- dim(x_UD)[2]
    nvar <- nvar + nvar_UD
  }
  assert_that(nvar > 0)


  # Create a variable information list and design matrix passed to glmnet
  vars_info <- list()
  x_for_glmnet <- NULL

  # Get O-dummies for each column of x
  if (nvar - nvar_UD > 0) {
    for (i in seq(nvar - nvar_UD)) {
      res <- getODummyMatForOneVec(x[, i])

      var_name <- names(x)[i]
      if (is.null(var_name)) var_name <- paste0("X_", i)
      var_info <- list(name=var_name,
                       type="O",
                       breaks=res$breaks)
      vars_info[[i]] <- var_info

      x_for_glmnet <- cbind(x_for_glmnet, res$dummy_mat)
    }
  }

  # Get U-dummies for each column of x_UD
  if (nvar_UD > 0) {
    for (i in seq(nvar_UD)) {
      res <- getUDummyMatForOneVec(x_UD[, i])

      var_name <- names(x_UD)[i]
      if (is.null(var_name)) var_name <- paste0("X_UD_", i)
      var_info <- list(name=var_name,
                       type="U",
                       levels=res$levels)
      vars_info[[nvar - nvar_UD + i]] <- var_info

      x_for_glmnet <- cbind(x_for_glmnet, res$dummy_mat)
    }
  }


  glmnet_result <- glmnet(x=x_for_glmnet, y=y,
                          family=family,
                          weights=weights,
                          offset=offset,
                          alpha=alpha,
                          nlambda=nlambda,
                          #lambda.min.ratio=lambda.min.ratio,
                          lambda=lambda,
                          standardize=standardize,
                          intercept=intercept,
                          thresh=thresh,
                          #dfmax=dfmax,
                          #pmax=pmax,
                          exclude=exclude,
                          #penalty.factor=penalty.factor,
                          lower.limits=lower.limits,
                          upper.limits=upper.limits,
                          maxit=maxit,
                          #type.gaussian=type.gaussian,
                          type.logistic=type.logistic,
                          standardize.response=standardize.response)

  ret <- list(backend_result=glmnet_result, vars_info=vars_info)
  class(ret) <- c(class(ret), "aglm")
  return(ret)
}
