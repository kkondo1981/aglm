# calculate deviances for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Extract coefficients from an `AccurateGLM` object.
#'
#' @param object An `AccurateGLM` object.
#' @param index An integer vector of variable indices whose coefficients should be returned.
#' @param name An character vector of variable names whose coefficients should be returned.
#' @param s  Value(s) of the penalty parameter `lambda` at which predictions are required.
#'   Default is the entire sequence used to create the model.
#' @param exact Same as `coef.glmnet()`.
#' @param ... Other arguments are passed directly to `deviance` functions of `model@backend_models`.
#'
#' @importFrom assertthat assert_that
#' @importFrom stats coef
#' @export
coef.AccurateGLM <- function(object, index=NULL, name=NULL, s=NULL, exact=FALSE, ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- object

  coefs <- coef(model@backend_models[[1]], s, exact, ...)

  # If `name` is set, `index` is overwritten.
  if (!is.null(name)) {
    tmp <- model@vars_info[sapply(model@vars_info, function(v){v$name}) == name]
    assert_that(length(tmp) == 1)
    index <- tmp[[1]]$idx
  }

  # If `index` or `name` is set, returns coefficients of that variable.
  if (!is.null(index)) {
    nvars <- length(model@vars_info)
    off0 <- 1  # not 0 because the first column is used as intercept.
    for (i in seq(nvars)) {
      var_info <- model@vars_info[[i]]
      ncol_linear <- 0
      if (var_info$use_linear) ncol_linear <- 1

      ncol_OD <- 0
      if(var_info$use_OD) {
        ncol_OD <- length(var_info$OD_info$breaks)
        if (var_info$OD_type == "C") ncol_OD <- ncol_OD - 1
      }

      ncol_UD <- 0
      if(var_info$use_UD) ncol_UD <- length(var_info$UD_info$levels) - var_info$UD_info$drop_last

      ncol_LV <- 0
      # In case length(LV_info$breaks) <= 2, there are no internal breaks, and so only a linear column is created.
      if(var_info$use_LV & length(var_info$LV_info$breaks) > 2) ncol_LV <- length(var_info$LV_info$breaks) - 2

      if (i == index) {
        c <- list()

        if(ncol_linear) c$coef.linear <- coefs[off0 + 1,] else c$coef.linear <- NULL
        off0 <- off0 + ncol_linear

        if(ncol_OD) c$coef.OD <- coefs[off0 + 1:ncol_OD, ] else c$coef.OD <-  NULL
        off0 <- off0 + ncol_OD

        if(ncol_UD) c$coef.UD <- coefs[off0 + 1:ncol_UD,] else c$coef.UD <- NULL
        off0 <- off0 + ncol_UD

        if(ncol_LV) c$coef.LV <- coefs[off0 + 1:ncol_LV, ] else c$coef.LV <-  NULL
        off0 <- off0 + ncol_LV

        return(c)
      } else {
        ncol_all <- ncol_linear + ncol_OD + ncol_UD + ncol_LV
        off0 <- off0 + ncol_all
      }
    }
    assert_that(FALSE)
  }

  # If neither `index` nor `name` is set, return coefficients of all variables
  return(coefs)
}
