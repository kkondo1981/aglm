# S4 class for fitted AGLM
# written by Kenji Kondo @ 2019/1/2


#' S4 class for fitted AGLM, used as a result of `aglm()` or `cv.aglm()`
#'
#' @slot backend_models Internally used model objects to be passed to backend functions.
#'   Currently `glmnet` is used as a backend and this slot holding a `glmnet` object.
#' @slot vars_info A list of list. Each element of `vars_info` represents one predictor variable and contains various information of it.
#' @slot lambda The values of `lambda` used in the fits.
#' @slot cvm The mean cross-validated error.
#' @slot cvsd The estimate of standard error of `cvm`.
#' @slot cvup The upper curve as `cvm + cvsd`.
#' @slot cvlo The lower curve as `cvm - cvsd`.
#' @slot nzero The number of non-zero coefficients at each lambda.
#' @slot name A text string indicating type of measure (for plotting purposes).
#' @slot lambda.min The value of `lambda` that gives minimum `cvm`.
#' @slot lambda.1se The largest value of `lambda` such that error is within 1 standard error of the minimum.
#' @slot fit.preval If `keep=TRUE`, this is the array of previously prevalidated fits. Some entries can be NA, if that and subsequent values of lambda are not reached for that fold.
#' @slot foldid An integer vector of values between 1 and `nfold` identifying what fold each observation is in.
#' @slot call An object of class call, which is used to record how `cva.aglm()` is called.
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

#' S4 class for a result of `cva.aglm()`
#'
#' @slot models_list Results of `cv.glmnet()` for all the values of alpha.
#' @slot alpha A numeric values specifying alpha values to be examined.
#' @slot nfolds An integer value specifying the number of folds.
#' @slot alpha.min.index An integer value specifying the index of `alpha.min` in `alpha`.
#' @slot alpha.min The alpha value which achieves the minimum loss.
#' @slot lambda.min The lambda value which achieves the minimum loss, when combined with `alpha.min`.
#' @slot call An object of class call, which is used to record how `cva.aglm()` is called.
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
