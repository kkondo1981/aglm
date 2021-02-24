# plotting function for AGLM
# written by Kenji Kondo @ 2019/1/3

#' Plot coefficients from an `AccurateGLM` object
#'
#' @param x An `AccurateGLM` object.
#' @param vars An integer or character vectors (indices or names) specifying which variables should be plotted.
#' @param verbose If TRUE, outputs details.
#' @param s A numeric value specifying lambda value at which plotting is required.
#'   Note that this function can't plot for multiple lambda values, so it allows only
#'   single `s` value (which means `model` is trained with multiple lambda values and plot with one of them),
#'   or `s=NULL` (which means `model` is trained with single lambda value and plot with that value).
#' @param resid A logical value which indicates to plot residuals,
#'   or a character value which indicates residual type to be plotted (see the help of `residuals.AccurateGLM()`),
#'   or a numerical vector which indicates residual values to be plotted.
#'   Note that working residuals are used in the first case with `resid=TRUE`.
#' @param smooth_resid A logical value which indicates whether draws smoothing lines of residuals or not,
#'   or a character value which is one of options below:
#'     * `"both"` draws both balls and smoothing lines.
#'     * `"smooth_only"` draws only smoothing line.
#'   Note that smoothing lines are only drawn for quantitative variables.
#'   The default value is `TRUE`.
#' @param smooth_resid_fun A function to be used to smooth partial residual values.
#' @param ask A logical value which indicates ask if go to next plot.
#' @param layout A pair of integer values which indicates how many plots are drawn row-wise and column-wise respectively,
#' @param only_plot If `TRUE`, the function set no graphical parameters and no title.
#' @param main A character value which indicates titles of panels.
#' @param add_rug A logical value which indicates draw rug plot for quantitative variables.
#' @param ... Other arguments are currently not used.
#'
#' @export
#' @importFrom assertthat assert_that
#' @importFrom utils str
#' @importFrom utils flush.console
#' @importFrom stats getCall
#' @importFrom stats residuals
#' @importFrom stats coef
#' @importFrom stats IQR
#' @importFrom stats smooth.spline
#' @importFrom stats ksmooth
#' @importFrom graphics par
#' @importFrom graphics points
#' @importFrom graphics lines
#' @importFrom graphics rug
#' @importFrom graphics mtext
#' @importFrom graphics boxplot
#' @importFrom graphics barplot
#' @importFrom grDevices devAskNewPage
plot.AccurateGLM <- function(x,
                             vars=NULL,
                             verbose=TRUE,
                             s=NULL,
                             resid=FALSE,
                             smooth_resid=TRUE,
                             smooth_resid_fun=NULL,
                             ask=TRUE,
                             layout=c(2,2),
                             only_plot=FALSE,
                             main="",
                             add_rug=FALSE,
                             ...) {
  # It's necessary to use same names for some arguments as the original methods,
  # because devtools::check() issues warnings when using inconsistent names.
  # As a result, we sometimes should accept uncomfortable argument names,
  # but still have rights to use preferable names internally.
  model <- x

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
  use_x.orig <- if (is.logical(resid)) resid else TRUE
  if (use_x.orig) {
    call.orig <- getCall(model)
    x.orig <- eval.parent(call.orig$x)
    if (class(x.orig)[1] != "data.frame")
      x.orig <- data.frame(x.orig)

    if (is.numeric(resid)) {
      resids <- resid
      resid <- TRUE
    } else if (is.character(resid)) {
      resids <- residuals(model, x=x.orig, s=s, type=resid)
      resid <- TRUE
    } else {
      resids <- residuals(model, x=x.orig, s=s, type="working")
    }
    assert_that(nrow(x.orig) == length(resids))
  }

  ## set flags for smoothing
  if (resid) {
    if (is.character(smooth_resid)) {
      draws_balls <- smooth_resid == "both"
      draws_lines <- TRUE
    } else {
      draws_balls <- TRUE
      draws_lines <- smooth_resid
    }
  }

  ## set par
  if (!only_plot) {
    old.par <- par()
    par(oma=c(0, 0, 2, 0))
    if (length(inds) == 1) layout <- c(1,1)
    par(mfrow=layout)
  }

  ask.old <- devAskNewPage()
  devAskNewPage(FALSE)

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
      if (var_info$use_LV) {
        breaks <- var_info$LV_info$breaks
      } else {
        breaks <- var_info$OD_info$breaks
      }
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
      if (var_info$use_LV) {
        b <- matrix(c(coefs$coef.linear, coefs$coef.LV), ncol=1)
      } else {
        b <- matrix(c(coefs$coef.linear, coefs$coef.OD), ncol=1)
      }
      comp <- drop(x.mat %*% b)

      ## Calculates component and residual values of samples
      x.sample <- NULL
      c_and_r.sample <- NULL
      if (resid) {
        x.sample <- x.orig[, var_info$data_column_idx]
        x.sample.mat <- getMatrixRepresentationByVector(x.sample, var_info)
        c_and_r.sample <- drop(x.sample.mat %*% b) + resids

        if (draws_lines) {
          ord <- order(x.sample)
          f <- smooth_resid_fun
          if (is.null(f)) {
            if (length(unique(x.sample)) >= 4 & IQR(x.sample) > 0)
              f <- smooth.spline
            else
              f <- ksmooth
          }
          smoothed_c_and_r.sample <- f(x.sample[ord], c_and_r.sample[ord])
        }
      }

      ## Plotting
      x.all <- c(x, x.sample)
      xlim <- c(min(x.all), max(x.all))
      xlim[1] <- xlim[1] - 0.05 * (xlim[2] - xlim[1])
      xlim[2] <- xlim[2] + 0.05 * (xlim[2] - xlim[1])

      y.all <- comp
      if (resid) {
        if (draws_balls)
          y.all <- c(y.all, c_and_r.sample)
        if (draws_lines)
          y.all <- c(y.all, smoothed_c_and_r.sample$y[!is.na(smoothed_c_and_r.sample$y)])
      }
      ylim <- c(min(y.all), max(y.all))
      ylim[1] <- ylim[1] - 0.05 * (ylim[2] - ylim[1])
      ylim[2] <- ylim[2] + 0.05 * (ylim[2] - ylim[1])

      plot(x=x,
           y=comp,
           type="n",
           main=main,
           xlab=xlab,
           ylab=ylab,
           xlim=xlim,
           ylim=ylim)

      if (resid) {
        if (draws_balls) {
          points(x=x.sample,
                 y=c_and_r.sample,
                 pch=".")
        }

        if (draws_lines) {
          lines(smoothed_c_and_r.sample,
                col="blue",
                lty=5)
        }

        if (add_rug) {
          rug(x=x.sample,
              col="gray")
        }
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

        boxplot(c_and_r.sample ~ x.sample,
                main=main,
                xlab=xlab,
                ylab=ylab,
                outline=FALSE)
      } else {
        barplot(comp,
                names=lv,
                main=main,
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
      if (!only_plot) {
        if (resid) mtext(line=0, outer=TRUE, text="Component + Residual Plot")
        else mtext(line=0, outer=TRUE, text="Component Plot")
      }
      devAskNewPage(ask)
      first <- FALSE
    }
  }
  devAskNewPage(ask.old)

  if (!only_plot) {
    if (!is.null(old.par$oma)) par(oma=old.par$oma)
    if (!is.null(old.par$mfrow)) par(mfrow=old.par$mfrow)
  }
}
