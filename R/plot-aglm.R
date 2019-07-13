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
#' @param resid A boolean value which indicates plot residuals.
#' @param ask A boolean value which indicates ask if go to next plot.
#' @param layout A pair of integer values which indicates how many plots are drawn rawwise and columnwise respectively.
#'
#' @export
plot.AccurateGLM <- function(model, vars=NULL, verbose=TRUE, s=NULL, resid=FALSE, ask=TRUE, layout=c(2,2), ...) {
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

  ## Calculates residuals
  if (resid) {
    call.orig <- getCall(model)
    x.orig <- eval(call.orig$x)
    if (class(x.orig) != "data.frame") x <- data.frame(x.orig)
    resids <- residuals(model, x=x.orig, s=s, type="working")
  }

  ## set par
  par(oma=c(0, 0, 2, 0))
  par(mfrow=layout)

  ## Plotting
  for (i in inds) {
    var_info <- model@vars_info[[i]]
    if (var_info$type == "inter") break ## no plot for interactions

    coefs <- coef(model, index=var_info$idx, s=s)

    if (resid) {
      xlab <- var_info$name
      ylab <- "Comp + Resid"
    } else {
      xlab <- var_info$name
      ylab <- "Comp"
    }

    first <- TRUE
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

      ## Calculates component values of x
      x.mat <- getMatrixRepresentationByVector(x, var_info)
      b <- matrix(c(coefs$coef.linear, coefs$coef.OD), ncol=1)
      comp <- drop(x.mat %*% b)

      ## Calculates component and residual values of samples
      x.sample <- NULL
      c_and_r.sample <- NULL
      if (resid) {
        x.sample <- x.orig[, var_info$data_column_idx]
        x.sample.mat <- getMatrixRepresentationByVector(x.sample, var_info)
        c_and_r.sample <- drop(x.sample.mat %*% b) + resids
      }

      ## Plotting
      x.all <- c(x, x.sample)
      xlim <- c(min(x.all), max(x.all))
      xlim[1] <- xlim[1] - 0.05 * (xlim[2] - xlim[1])
      xlim[2] <- xlim[2] + 0.05 * (xlim[2] - xlim[1])

      y.all <- c(comp, c_and_r.sample)
      ylim <- c(min(y.all), max(y.all))
      ylim[1] <- ylim[1] - 0.05 * (ylim[2] - ylim[1])
      ylim[2] <- ylim[2] + 0.05 * (ylim[2] - ylim[1])

      plot(x=x,
           y=comp,
           type="n",
           xlab=xlab,
           ylab=ylab,
           xlim=xlim,
           ylim=ylim)

      if (resid) {
        points(x=x.sample,
               y=c_and_r.sample,
               pch=".")
      }

      lines(x=x,
            y=comp)
    } else if (var_info$type == "qual") {
      # Plot for factorial features

      ## All levels to be plotted
      lv <- var_info$UD_info$levels
      x <- if (var_info$use_OD) {
        ordered(lv, levels=lv)
      } else {
        factor(lv, levels=lv)
      }

      ## Calculate component values of x
      x.mat <- getMatrixRepresentationByVector(x, var_info)
      b <- matrix(c(coefs$coef.OD, coefs$coef.UD), ncol=1)
      comp <- drop(x.mat %*% b)

      # Calculates component and residual values of samples
      x.sample <- NULL
      c_and_r.sample <- NULL
      if (resid) {
        x.sample <- x.orig[, var_info$data_column_idx]
        x.sample <- if (var_info$use_OD) {
          ordered(x.sample, levels=lv)
        } else {
          factor(x.sample, levels=lv)
        }
        x.sample.mat <- getMatrixRepresentationByVector(x.sample, var_info)
        c_and_r.sample <- drop(x.sample.mat %*% b) + resids
      }

      if (resid) {
        y.all <- c(comp, c_and_r.sample)
        ylim <- c(min(y.all), max(y.all))
        ylim[1] <- ylim[1] - 0.05 * (ylim[2] - ylim[1])
        ylim[2] <- ylim[2] + 0.05 * (ylim[2] - ylim[1])

        boxplot(c_and_r.sample ~ x.sample,
                xlab=xlab,
                ylab=ylab,
                ylim=ylim)
      } else {
        barplot(comp,
                names=lv,
                xlab=xlab,
                ylab=ylab)
      }
    }

    if (verbose) {
      cat(sprintf("Plotting for %s", var_info$name))
      cat("Variable Informations:\n"); str(var_info); cat("\n")
      cat("Coefficients:\n"); str(coefs); cat("\n")
    }

    flush.console() # this makes sure that the display is current


    if (first) {
      if (resid) mtext(line=0, outer=TRUE, text="Component + Residual Plot")
      else mtext(line=0, outer=TRUE, text="Component Plot")
      if (ask) devAskNewPage(TRUE)
      first <- FALSE
    }
  }
  if (ask) devAskNewPage(FALSE)
}
