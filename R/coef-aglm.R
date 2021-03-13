# calculate deviances for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Extract coefficients from an AccurateGLM object.
#'
#' @param model An AccurateGLM object.
#' @param s  Value(s) of the penalty parameter `lambda` at which predictions are required.
#'   Default is the entire sequence used to create the model.
#' @param ... Other arguments are passed directly to `deviance` functions of `model@backend_models`.
#'
#' @importFrom assertthat assert_that
#' @export
coef.AccurateGLM <- function(model, index=NULL, name=NULL, s=NULL, exact=FALSE, ...) {
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
    off0 <- ifelse(model@call$family == "cox", 0, 1)  # the first column (0) is used as intercept other than cox
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
