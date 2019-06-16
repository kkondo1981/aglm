# S4 class for fitted AGLM
# written by Kenji Kondo @ 2019/1/2


#' S4 class for fitted AGLM
#'
#' @slot backend_models Internally used model objects to be passed to backend functions.
#'   Currently glmnet is used as a backend and this slot holding a glmnet object.
#' @slot vars_info A list of list. Each element of `vars_info` represents one predictor variable and contains various informations of it.
#' @slot others slots for holding cross-validation results
#'
#' @export
setClass("AccurateGLM",
         representation=representation(backend_models="list",
                                       vars_info="list",
                                       lambda="numeric",
                                       cvm="numeric",
                                       cvsd="numeric",
                                       cvup="numeric",
                                       cvlo="numeric",
                                       nzero="integer",
                                       name="character",
                                       lambda.min="numeric",
                                       lambda.1se="numeric",
                                       fit.preval="matrix",
                                       foldid="integer",
                                       call="ANY"))

#' S4 class for the result of cva.aglm function
#'
#' @slot models_list Results of cv.glmnet() for all the values of alpha.
#' @slot alpha A numeric values specifying alpha values to be examined.
#' @slot nfolds An integer value specifying the number of folds.
#' @slot alpha.min The alpha value which achieves the minimum loss.
#' @slot alpha.min.index An integer value specifying the index of `alpha.min` in `alpha`.
#' @slot lambda.min The lambda value which achieves the minimum loss, when combined with `alpha.min`.
#'
#' @export
setClass("CVA_AccurateGLM",
         representation=representation(models_list="list",
                                       alpha="numeric",
                                       nfolds="integer",
                                       alpha.min.index="integer",
                                       alpha.min="numeric",
                                       lambda.min="numeric",
                                       call="ANY"))
