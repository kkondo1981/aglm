# fitting function for AGLM model
# written by Kenji Kondo @ 2019/1/1

#' fit an AGLM model
#'
#' @param x input matrix or data.frame of size (nobs, _). Each row shold be an integer or numeric vector.
#' @param x_UD additional input matrix of size (nbos, _). Each row should be an integer or character or factor vector.
#' @param y response variable.
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
aglm <- function(x, x_UD, y,
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
  assert_that(class(x_UD) == "matrix" | class(x_UD) == "data.frame")
  assert_that(dim(x)[1] == dim(x_UD)[1])

  y <- drop(y)
  assert_that(class(y) == "integer" | class(y) == "numeric")
  assert_that(length(y) == dim(x)[1])

  n <- dim(x)[1]
  nvar <- dim(x)[2] + dim(x_UD)[2]
  nvar_UD <- dim(x_UD)[2]


  # Create a variable information list and design matrix passed to glmnet
  vars_info <- list()
  x_for_glmnet <- NULL

  # Get O-dummies for each column of x
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

  # Get U-dummies for each column of x_UD
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
