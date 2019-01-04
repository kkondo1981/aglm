# S4 class for fitted AGLM
# written by Kenji Kondo @ 2019/1/2


#' S4 class for fitted AGLM
#'
#' @slot backend_models Internally used model objects to be passed to backend functions.
#'   Currently glmnet is used as a backend and this slot holding a glmnet object.
#' @slot vars_info A list of list. Each element of `vars_info` represents one predictor variable and contains various informations of it.
#'
#' @export
setClass("AccurateGLM",
         representation=representation(backend_models="list", vars_info="list"))
