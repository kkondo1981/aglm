#' Class for results of `aglm()` and `cv.aglm()`
#'
#' @slot backend_models The fitted backend `glmnet` model is stored.
#' @slot vars_info A list, each of whose element is information of one variable.
#' @slot lambda Same as in the result of \link{cv.glmnet}.
#' @slot cvm Same as in the result of \link{cv.glmnet}.
#' @slot cvsd Same as in the result of \link{cv.glmnet}.
#' @slot cvup Same as in the result of \link{cv.glmnet}.
#' @slot cvlo Same as in the result of \link{cv.glmnet}.
#' @slot nzero Same as in the result of \link{cv.glmnet}.
#' @slot name Same as in the result of \link{cv.glmnet}.
#' @slot lambda.min Same as in the result of \link{cv.glmnet}.
#' @slot lambda.1se Same as in the result of \link{cv.glmnet}.
#' @slot fit.preval Same as in the result of \link{cv.glmnet}.
#' @slot foldid Same as in the result of \link{cv.glmnet}.
#' @slot call An object of class `call`, corresponding to the function call when this `AccurateGLM` object is created.
#'
#' @author Kenji Kondo
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


#' Class for results of `cva.aglm()`
#'
#' @slot models_list A list consists of `cv.glmnet()`'s results for all \eqn{\alpha} values.
#' @slot alpha Same as in \link{cv.aglm}.
#' @slot nfolds Same as in \link{cv.aglm}.
#' @slot alpha.min.index The index of `alpha.min` in the vector `alpha`.
#' @slot alpha.min The \eqn{\alpha} value achieving the minimum loss among all the values of `alpha`.
#' @slot lambda.min The \eqn{\lambda} value achieving the minimum loss when \eqn{\alpha} is equal to `alpha.min`.
#' @slot call An object of class `call`, corresponding to the function call when this `CVA_AccurateGLM` object is created.
#'
#' @author Kenji Kondo
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
