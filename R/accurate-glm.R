# S4 class for fitted AGLM
# written by Kenji Kondo @ 2019/1/2


#' S4 class for fitted AGLM
#'
#' @slot backend_model A model object to be passed to backend API internally. Currently glmnet is used as backend,
#'   and this slot holding a glmnet object.
#' @slot vars_info A list of lists. Each element represents one predictor and contains information
#'   of it and how to create its matrix representation from `data` slot.
#'
#' @export
setClass("AccurateGLM",
         representation=representation(backend_models="list", vars_info="list"))
