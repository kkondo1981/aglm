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

    if (is_multinomial)
      coefs <- coef(model, index=var_info$idx, s=s, class=cl)
    else
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
        if (is_multinomial) text <- paste0(text, " (class=", cl, ")")
        mtext(line=0, outer=TRUE, text=text)
      }
      devAskNewPage(ask)
      first <- FALSE
    }
  }
}
