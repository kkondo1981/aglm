# plotting function for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Plot coefficients from an AccurateGLM object
#'
#' @param model An AccurateGLM object.
#' @param vars An integer or character vectors (indices or names) specifying which variables should be plotted.
#' @param verbose If TRUE, outputs details.
#' @param s A numeric value specifying lambda value at which plotting is required.
#'   Note that this function can't plot for multiple lambda values, so it allows only
#'   single `s` value (which means `model` is trained with multiple lambda values and plot with one of them),
#'   or `s=NULL` (which means `model` is trained with single lambda value and plot with that value).
#'
#' @export
plot.AccurateGLM <- function(model, vars=NULL, verbose=TRUE, s=NULL, ...) {
  coefs_all <- coef(model, s=s)
  nvars <- length(model@vars_info)

  if (is.null(vars)) {
    inds <- seq(nvars)
  } else if (is.numeric(vars)) {
    inds <- unique(sort(vars))
  } else if (is.character(vars)) {
    inds <- NULL
    for (i in seq(nvars)) {
      var_name <- model@vars_info[[i]]$name
      if (var_name %in% vars) inds <- c(inds, i)
    }
  } else {
    assert_that(FALSE)
  }

  devAskNewPage(TRUE)
  for (i in inds) {
    var_info <- model@vars_info[[i]]
    if (var_info$type == "inter") break ## no plot for interactions

    coefs <- coef(model, index=var_info$idx, s=s)
    main <- sprintf("Cofficients Plot for variable `%s`", var_info$name)

    if (var_info$type == "quan") {
      # Plot for numeric features

      ## Calculates range of x to be plotted
      breaks <- var_info$OD_info$breaks
      breaks <- breaks[abs(breaks) < Inf]  # get rid of -Inf and Inf
      x.min <- min(breaks)
      x.max <- max(breaks)
      x.d <- x.max - x.min
      assert_that(x.d > 0)

      x.min <- x.min - 0.05 * x.d
      x.max <- x.max + 0.05 * x.d
      x.d <- x.max - x.min

      ## Extract x values to be plotted
      x <- x.min + (0:2000) / 2000 * x.d

      ## Calculates component values for x
      x.mat <- getMatrixRepresentationByVector(x, var_info)
      b <- matrix(c(coefs$coef.linear, coefs$coef.OD), ncol=1)
      comp <- x.mat %*% b
      type <- "l"

      plot(x=x, y=comp,
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
