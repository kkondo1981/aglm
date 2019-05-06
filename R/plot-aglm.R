# plotting function for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Plot coefficients from an AccurateGLM object
#'
#' @param model An AccurateGLM object.
#' @param verbose If TRUE, outputs details.
#' @param s A numeric value specifying lambda value at which plotting is required.
#'   Note that this function can't plot for multiple lambda values, so it allows only
#'   single `s` value (which means `model` is trained with multiple lambda values and plot with one of them),
#'   or `s=NULL` (which means `model` is trained with single lambda value and plot with that value).
#'
#' @export
plot.AccurateGLM <- function(model, verbose=TRUE, s=NULL, ...) {
  coefs_all <- coef(model, s=s)
  nvars <- length(model@vars_info)

  devAskNewPage(TRUE)
  for (i in seq(nvars)) {
    var_info <- model@vars_info[[i]]
    if (var_info$type == "inter") break ## no plot for interactions

    coefs <- coef(model, index=var_info$idx, s=s)
    main <- sprintf("Cofficients Plot for variable `%s`", var_info$name)

    if (var_info$type == "quan") {
      # Plot for numeric features
      slope <- coefs$coef.linear
      steps <- coefs$coef.OD
      if (is.null(slope)) slope <- 0
      if (is.null(steps)) steps <- 0

      if (var_info$OD_type == 'C') {
        x <- var_info$OD_info$breaks
        y <- slope * x + cumsum(c(0, steps))
        type <- "l"
      } else if (var_info$OD_type == 'J') {
        x <- var_info$OD_info$breaks
        y <- slope * x + cumsum(steps)
        type <- ifelse(slope == 0, "s", "l")
      } else {
        assert_that(FALSE)
      }

      plot(x=x, y=y,
           type=type,
           main=main,
           xlab=var_info$name,
           ylab="Coefficients")

    } else if (var_info$type == "qual") {
      # Plot for factorial features
      steps <- coefs$coef.OD
      levels <- coefs$coef.UD
      if (is.null(steps)) steps <- 0
      if (is.null(levels)) leels <- 0

      y <- cumsum(steps) + levels
      names <- var_info$OD_info$breaks

      barplot(y,
              names.arg = names,
              main=main,
              xlab = var_info$name,
              ylab = "Coefficients")
    }

    if (verbose) {
      cat(main); cat("\n\n")
      cat("Variable Informations:\n"); str(var_info); cat("\n")
      cat("Coefficients:\n"); str(coefs); cat("\n")
    }

    flush.console() # this makes sure that the display is current
  }
  devAskNewPage(FALSE)
}
