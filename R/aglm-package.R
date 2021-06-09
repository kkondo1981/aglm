#' aglm: Accurate Generalized Linear Model
#'
#' Provides functions to fit Accurate Generalized Linear Model (AGLM) models,
#' visualize them, and predict for new data. AGLM is defined as a regularized GLM
#' which applies a sort of feature transformations using a discretization of numerical
#' features and specific coding methodologies of dummy variables.
#' For more information on AGLM, see
#' \href{https://www.institutdesactuaires.com/global/gene/link.php?doc_id=16273&fg=1}{Suguru Fujita, Toyoto Tanaka, Kenji Kondo and Hirokazu Iwasawa (2020)}.
#'
#' The collection of functions provided by the `aglm` package has almost the same structure as the famous `glmnet` package,
#' so users familiar with the `glmnet` package will be able to handle it easily.
#' In fact, this structure is reasonable in implementation, because what the `aglm` package does is
#' applying appropriate transformations to the given data and passing it to the `glmnet` package as a backend.
#'
#' @section Fitting functions:
#' The `aglm` package provides three different fitting functions, depending on how users want to handle hyper-parameters of AGLM models.
#'
#' Because AGLM is based on regularized GLM, the regularization term of the loss function can be expressed as follows:
#' \loadmathjax
#' \mjsdeqn{
#'   R(\lbrace \beta_{jk} \rbrace; \lambda, \alpha)
#'    = \lambda \left\lbrace
#'      (1 - \alpha)\sum_{j=1}^{p} \sum_{k=1}^{m_j}|\beta_{jk}|^2 + \alpha \sum_{j=1}^{p} \sum_{k=1}^{m_j} |\beta_{jk}|
#'    \right\rbrace,
#' }
#' where \eqn{\beta_jk} is the k-th coefficient of auxiliary variables for the j-th column in data,
#' \eqn{\alpha} is a weight which controls how L1 and L2 regularization terms are mixed,
#' and \eqn{\lambda} determines the strength of the regularization.
#'
#' Searching hyper-parameters \eqn{\alpha} and \eqn{\lambda} is often useful to get better results, but usually time-consuming.
#' That's why the `aglm` package provides three fitting functions with different strategies for specifying hyper-parameters as follows:
#'   * \link{aglm}: A basic fitting function with given \eqn{\alpha} and \eqn{\lambda} (s).
#'   * \link{cv.aglm}: A fitting function with given \eqn{\alpha} and cross-validation for \eqn{\lambda}.
#'   * \link{cva.aglm}: A fitting function with cross-validation for both \eqn{\alpha} and \eqn{\lambda}.
#'
#' Generally speaking, setting an appropriate \eqn{\lambda} is often important to get meaningful results,
#' and using `cv.aglm()` with default \eqn{\alpha=1} (LASSO) is usually enough.
#' Since `cva.aglm()` is much time-consuming than `cv.aglm()`, it is better to use it only if particularly better results are needed.
#'
#' The following S4 classes are defined to store results of the fitting functions.
#'   * \link{AccurateGLM-class}: A class for results of `aglm()` and `cv.aglm()`
#'   * \link{CVA_AccurateGLM-class}: A class for results of `cva.aglm()`
#'
#' @section Using the fitted model:
#' Users can use models obtained from fitting functions in various ways, by passing them to following functions:
#'   * \link[=predict.AccurateGLM]{predict}: Make predictions for new data
#'   * \link[=plot.AccurateGLM]{plot}: Plot contribution of each variable and residuals
#'   * \link[=print.AccurateGLM]{print}: Display textual information of the model
#'   * \link[=coef.AccurateGLM]{coef}: Get coefficients
#'   * \link[=deviance.AccurateGLM]{deviance}: Get deviance
#'   * \link[=residuals.AccurateGLM]{residuals}: Get residuals of various types
#'
#' We emphasize that `plot()` is particularly useful to understand the fitted model,
#' because it presents a visual representation of how variables in the original data are used by the model.
#'
#' @section Other functions:
#' The following functions are basically for internal use, but exported as utility functions for convenience.
#'
#' * Functions for creating feature vectors
#'   * \link{getUDummyMatForOneVec}
#'   * \link{getODummyMatForOneVec}
#'   * \link{getLVarMatForOneVec}
#' * Functions for binning
#'   * \link{createEqualWidthBins}
#'   * \link{createEqualFreqBins}
#'   * \link{executeBinning}
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
#' @docType package
#' @name aglm-package
NULL
