#' Plot contribution of each variable and residuals
#'
#' @param x
#'   A model object obtained from `aglm()` or `cv.aglm()`.
#'
#' @param vars
#'   Used to specify variables to be plotted (`NULL` means all the variables).
#'   This parameter may have one of the following classes:
#'   * `integer`: specifying variables by index.
#'   * `character`: specifying variables by name.
#'
#' @param verbose
#'   Set to `FALSE` if textual outputs are not needed.
#'
#' @param s
#'   A numeric value specifying \eqn{\lambda} at which plotting is required.
#'   Note that plotting for multiple \eqn{\lambda}'s are not allowed and `s` always should be a single value.
#'   When the model is trained with only a single \eqn{\lambda} value, just set it to `NULL` to plot for that value.
#'
#' @param resid
#'   Used to display residuals in plots.
#'   This parameter may have one of the following classes:
#'   * `logical`(single value): If `TRUE`, working residuals are plotted.
#'   * `character`(single value): type of residual to be plotted. See \link{residuals.AccurateGLM} for more details on types of residuals.
#'   * `numerical`(vector): residual values to be plotted.
#'
#' @param smooth_resid
#'   Used to display smoothing lines of residuals for quantitative variables.
#'   This parameter may have one of the following classes:
#'   * `logical`: If `TRUE`, smoothing lines are drawn.
#'   * `character`:
#'     * `smooth_resid="both"`: Balls and smoothing lines are drawn.
#'     * `smooth_resid="smooth_only"`: Only smoothing lines are drawn.
#'
#' @param smooth_resid_fun
#'   Set if users need custom smoothing functions.
#'
#' @param ask
#'   By default, `plot()` stops and waits inputs each time plotting for each variable is completed.
#'   Users can set `ask=FALSE` to avoid this.
#'   It is useful, for example, when using devices as `bmp` to create image files.
#'
#' @param layout
#'   Plotting multiple variables for each page is allowed.
#'   To achieve this, set it to a pair of integer, which indicating number of rows and columns, respectively.
#'
#' @param only_plot
#'   Set to `TRUE` if no automatic graphical configurations are needed.
#'
#' @param main
#'   Used to specify the title of plotting.
#'
#' @param add_rug
#'   Set to `TRUE` for rug plots.
#'
#' @param class
#'   Used to specify which class to be plotted in the case of multinomial regression.
#'   `class=NULL` means plotting all classes.
#'
#' @param use_legend
#'   Set to `TRUE` if a legend is needed.
#'   This parameter only works in multinomial cases.
#'
#' @param col
#'   Used to specify colors of contribution curves.
#'   In the case of multinomial regression, specify colors for all classes by a vector.
#'
#' @param margin
#'   A numerical vector of the form `c(bottom, left, top, right)`.
#'   Each element means a rate that indicates how much plot areas should be extended
#'   in the specified direction.
#'
#' @param ...
#'   Other arguments are currently not used and just discarded.
#'
#' @return
#'   No return value, called for side effects.
#'
#'
#' @example examples/predict-and-plot-1.R
#'
#'
#' @author
#'   * Kenji Kondo,
#'   * Kazuhisa Takahashi and Hikari Banno (worked on L-Variable related features)
#'
#'
#' @references Suguru Fujita, Toyoto Tanaka, Kenji Kondo and Hirokazu Iwasawa. (2020)
#' \emph{AGLM: A Hybrid Modeling Method of GLM and Data Science Techniques}, \cr
#' \url{https://www.institutdesactuaires.com/global/gene/link.php?doc_id=16273&fg=1} \cr
#' \emph{Actuarial Colloquium Paris 2020}
#'
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
                             class=NULL,
                             use_legend=FALSE,
                             col=NULL,
                             margin=rep(0.05, 4),
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

  ## For multinomial regressions, set which class should be plotted.
  is_multinomial <- !is.null(model@call$family) && model@call$family == "multinomial"
  if (is_multinomial) {
    if (is.null(class))
      class <- model@backend_models[[1]]$classnames
  } else {
    class <- 1  # only one iteration, the value is ignored.
  }
  n_class <- length(class)

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
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par), add=TRUE)
  }

  ask.old <- devAskNewPage()
  on.exit(devAskNewPage(ask.old), add=TRUE)

  ## set par
  if (!only_plot) {
    par(oma=c(0, 0, 2, 0))
    if (length(inds) == 1) layout <- c(1,1)
    par(mfrow=layout)
  }

  devAskNewPage(FALSE)

  ## Plotting
  for (i in inds) {
    var_info <- model@vars_info[[i]]
    if (var_info$type == "inter") break ## no plot for interactions

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
      comp_infos <- vector(mode="list", n_class)
      for (class_ind in seq(n_class)) {
        if (is_multinomial)
          coefs <- coef(model, index=var_info$idx, s=s, class=class[class_ind])
        else
          coefs <- coef(model, index=var_info$idx, s=s)
        if (var_info$use_LV) {
          b <- matrix(c(coefs$coef.linear, coefs$coef.LV), ncol=1)
        } else {
          b <- matrix(c(coefs$coef.linear, coefs$coef.OD), ncol=1)
        }
        comp_infos[[class_ind]]$b <- b
        comp_infos[[class_ind]]$comp <- drop(x.mat %*% b)
      }

      ## Calculates component and residual values of samples
      x.sample <- NULL
      c_and_r.sample <- NULL
      if (resid) {
        x.sample <- x.orig[, var_info$data_column_idx]
        x.sample.mat <- getMatrixRepresentationByVector(x.sample, var_info)
        for (class_ind in seq(n_class)) {
          b <- comp_infos[[class_ind]]$b
          comp_infos[[class_ind]]$c_and_r.sample <- drop(x.sample.mat %*% b) + resids
        }

        if (draws_lines) {
          ord <- order(x.sample)
          f <- smooth_resid_fun
          if (is.null(f)) {
            if (length(unique(x.sample)) >= 4 & IQR(x.sample) > 0)
              f <- smooth.spline
            else
              f <- ksmooth
          }
          for (class_ind in seq(n_class)) {
            c_and_r.sample <- comp_infos[[class_ind]]$c_and_r.sample
            comp_infos[[class_ind]]$smoothed_c_and_r.sample <- f(x.sample[ord], c_and_r.sample[ord])
          }
        }
      }

      ## Plotting
      x.all <- c(x, x.sample)
      xlim <- c(min(x.all), max(x.all))
      if (xlim[2] - xlim[1] < 1e-8) xlim <- c(-1, 1)
      xdelta <- xlim[2] - xlim[1]
      xlim[1] <- xlim[1] - margin[2] * xdelta
      xlim[2] <- xlim[2] + margin[4] * xdelta

      y.all <- NULL
      for (class_ind in seq(n_class)) {
        ci <- comp_infos[[class_ind]]
        y.all <- c(y.all, ci$comp)
        if (resid) {
          if (draws_balls)
            y.all <- c(y.all, ci$c_and_r.sample)
          if (draws_lines) {
            tmp <- ci$smoothed_c_and_r.sample
            y.all <- c(y.all, tmp$y[!is.na(tmp$y)])
          }
        }
      }
      ylim <- c(min(y.all), max(y.all))
      if (ylim[2] - ylim[1] < 1e-8) ylim <- c(-1, 1)
      ydelta <- ylim[2] - ylim[1]
      ylim[1] <- ylim[1] - margin[1] * ydelta
      ylim[2] <- ylim[2] + margin[3] * ydelta

      plot(x=0,
           y=0,
           type="n",
           main=main,
           xlab=xlab,
           ylab=ylab,
           xlim=xlim,
           ylim=ylim)

      if (resid) {
        for (class_ind in seq(n_class)) {
          ci <- comp_infos[[class_ind]]
          if (draws_balls) {
            points(x=x.sample,
                   y=ci$c_and_r.sample,
                   pch=".")
          }

          if (draws_lines) {
            lines(ci$smoothed_c_and_r.sample,
                  col="blue",
                  lty=5)
          }

          if (add_rug) {
            rug(x=x.sample,
                col="gray")
          }
        }
      }

      if (is.null(col)) {
        col1 <- rep("black", n_class)
      } else {
        col1 <- rep_len(col, n_class)
      }

      for (class_ind in seq(n_class)) {
        ci <- comp_infos[[class_ind]]
        lines(x=x,
              y=ci$comp,
              lty=class_ind,
              col=col1[class_ind])
      }

      if (is_multinomial && use_legend)
        legend(x=xlim[1] + 0.01 * xdelta,
               y=ylim[2] - 0.01 * ydelta,
               legend=class,
               cex=0.8,
               lty=seq(n_class),
               col=col1,
               horiz=TRUE)
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
      comp_infos <- vector(mode="list", n_class)
      for (class_ind in seq(n_class)) {
        if (is_multinomial)
          coefs <- coef(model, index=var_info$idx, s=s, class=class[class_ind])
        else
          coefs <- coef(model, index=var_info$idx, s=s)
        b <- matrix(c(coefs$coef.OD, coefs$coef.UD), ncol=1)
        comp_infos[[class_ind]]$b <- b
        comp_infos[[class_ind]]$comp <- drop(x.mat %*% b)
      }

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

        for (class_ind in seq(n_class)) {
          b <- comp_infos[[class_ind]]$b
          comp_infos[[class_ind]]$c_and_r.sample <- drop(x.sample.mat %*% b) + resids
        }
      }

      y.all <- NULL
      for (class_ind in seq(n_class)) {
        ci <- comp_infos[[class_ind]]
        y.all <- c(y.all, ci$comp)
        if (resid) {
          xvals <- unique(x.sample)
          for (xval in xvals) {
            yy <- ci$c_and_r.sample[x.sample == xval]
            y.all <- c(y.all, boxplot.stats(yy)$stats)
          }
        }
      }
      ylim <- c(min(y.all), max(y.all))
      if (ylim[2] - ylim[1] < 1e-8) ylim <- c(-1, 1)
      ydelta <- ylim[2] - ylim[1]
      ylim[1] <- ylim[1] - margin[1] * ydelta
      ylim[2] <- ylim[2] + margin[3] * ydelta

      if (resid) {
        # `resid` never get `TRUE` when multinomial case, so we can assume `n_class=1` here.
        ci <- comp_infos[[1]]

        # Emulates the calculation of `xlim` in `boxplot()` to apply `margin`.
        # See the original codes by The R Core Team at the URL below for details:
        # https://github.com/SurajGupta/r-source/blob/master/src/library/graphics/R/boxplot.R
        xlim <- range(1:length(unique(x.sample)), finite=TRUE) + c(-0.5, 0.5)
        xdelta <- xlim[2] - xlim[1]
        xlim[1] <- xlim[1] - margin[2] * xdelta
        xlim[2] <- xlim[2] + margin[4] * xdelta

        boxplot(ci$c_and_r.sample ~ x.sample,
                col=col,
                pars=list(xlim=xlim, ylim=ylim),
                main=main,
                xlab=xlab,
                ylab=ylab,
                outline=FALSE)
      } else {
        if (is_multinomial) {
          beside <- TRUE
          comp <- matrix(0, length(comp_infos), length(comp_infos[[1]]$comp))
          for (class_ind in seq(n_class))
            comp[class_ind, ] <- comp_infos[[class_ind]]$comp
        } else {
          beside <- FALSE
          comp <- comp_infos[[1]]$comp
        }

        # Emulates the calculation of `xlim` in `barplot()` to apply `margin`.
        # See the original codes by The R Core Team at the URL below for details:
        # https://github.com/SurajGupta/r-source/blob/master/src/library/graphics/R/barplot.R
        if (beside) {
          nr <- nrow(comp)
          nc <- ncol(comp)
          space <- rep.int(c(1, rep.int(0, nr - 1)), nc)
          width <- rep_len(1, nr)
        } else {
          space <- 0.2
          width <- rep_len(1, length(comp))
        }
        bar.right <- cumsum(space + width)
        bar.left <- bar.right - width
        xlim <- c(min(bar.left), max(bar.right))
        xdelta <- xlim[2] - xlim[1]
        xlim[1] <- xlim[1] - margin[2] * xdelta
        xlim[2] <- xlim[2] + margin[4] * xdelta

        if (is_multinomial && use_legend) {
          legend.text <- class
          args.legend <- list(x=xlim[1] + 0.01 * xdelta,
                              y=ylim[2] - 0.01 * ydelta,
                              cex=0.8,
                              xjust=0,
                              horiz=TRUE)
        } else {
          legend.text <- NULL
          args.legend <- NULL
        }

        barplot(comp,
                beside=beside,
                names.arg=lv,
                legend.text=legend.text,
                args.legend=args.legend,
                xlim=xlim,
                ylim=ylim,
                col=col,
                main=main,
                xlab=xlab,
                ylab=ylab)
      }
    }

    if (verbose) {
      cat(sprintf("Plotting for %s", var_info$name))
      if (is_multinomial)
        cat(sprintf(", class=%s", cl))
      cat("\n")
      cat("Variable Informations:\n"); str(var_info); cat("\n")
      cat("Coefficients:\n"); str(coefs); cat("\n")
    }

    flush.console() # this makes sure that the display is current


    if (first) {
      if (!only_plot) {
        if (resid) text <- "Component + Residual Plot"
        else text <- "Component Plot"
        mtext(line=0, outer=TRUE, text=text)
      }
      devAskNewPage(ask)
      first <- FALSE
    }
  }
}
