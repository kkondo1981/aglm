# handling inputs of AGLM model
# written by Kenji Kondo @ 2019/1/2


#' S4 class for input
#'
#' @slot vars_info A list of list. Each element has some information of one feature.
#' @slot data A data.frame which contains original data itself.
setClass("AGLM_Input",
         representation=representation(vars_info="list", data="data.frame"))


#' Create a new AGLM_Input object
#' @importFrom assertthat assert_that
newInput <- function(x=NULL,
                     qualitative_vars_UD_only=NULL,
                     qualitative_vars_both=NULL,
                     qualitative_vars_OD_only=NULL,
                     quantitative_vars=NULL,
                     add_linear_columns=TRUE,
                     add_OD_columns_of_qualitatives=TRUE,
                     add_intersection_columns=TRUE) {
  # Check and process arguments
  assert_that(!is.null(x))
  assert_that(class(x) == "matrix" | class(x) == "data.frame")
  if (class(x) == "matrix") x <- data.frame(x)
  assert_that(dim(x)[2] > 0)


  # Calculate data size
  nobs <- dim(x)[1]
  nvar <- dim(x)[2]
  assert_that(nobs > 0 & nvar > 0)


  # Create a list  (explanatory) variables' information.
  # Firstly create without considering qualitative_vars_UD_only, qualitative_vars_both, ...
  vars_info <- list()

  for (i in seq(nvar)) {
    var <- list()
    var$idx <- i
    var$name <- names(x)[i]
    var$data_column_idx <- i

    var$type <- if (is.factor(x[, i])) {"qual"} else {"quan"}
    is_ordered <- (var$type == "qual" & is.ordered(x[, i])) | (var$type == "quan")

    var$use_linear <- var$type == "quan" & add_linear_columns
    var$use_UD <- var$type == "qual"
    var$use_OD <- var$type == "quan" | (var$type == "qual" & is_ordered & add_OD_columns_of_qualitatives)

    vars_info[[i]] <- var
  }


  # Define a utility function
  var_names <- sapply(vars_info, function(var) {return(var$name)})
  get_idx <- function(idxs_or_names) {
    if (is.null(idxs_or_names)) {
      return(integer(0))
    }

    cl <- class(idxs_or_names)
    idxs <- seq(length(var_names))
    if (cl == "integer") {
      is_hit <- function(idx) {return(idx %in% idxs_or_names)}
      idxs <- idxs[sapply(idxs, is_hit)]
    } else if (cl == "character") {
      is_hit <- function(var_name) {return(var_name %in% idxs_or_names)}
      idxs <- idxs[sapply(var_names, is_hit)]
    } else {
      assert_that(FALSE, msg="qualitative_vars_UD_only, qualitative_vars_both, qualitative_vars_both, quantitative_vars should be integer or character vectors.")
    }
  }


  # Get indices of variables specified by qualitative_vars_UD_only, qualitative_vars_both, ...
  qual_UD <- get_idx(qualitative_vars_UD_only)
  qual_OD <- get_idx(qualitative_vars_both)
  qual_both <- get_idx(qualitative_vars_both)
  quan <- get_idx(quantitative_vars)


  # Check if no variables are doubly counted.
  msg <- paste0("Each pair of qualitative_vars_UD_only, qualitative_vars_both, qualitative_vars_both, ",
                "and quantitative_vars shouldn't be overlapped.")
  assert_that(length(qual_UD, qual_OD) == 0, msg)
  assert_that(length(qual_UD, qual_both) == 0, msg)
  assert_that(length(qual_UD, quan) == 0, msg)
  assert_that(length(qual_OD, qual_both) == 0, msg)
  assert_that(length(qual_OD, quan) == 0, msg)
  assert_that(length(qual_both, quan) == 0, msg)


  # Modify vars_info using qualitative_vars_UD_only, qualitative_vars_both, ...
  for (i in qual_UD) {
    var <- vars_info[[i]]
    var$type <- "qual"

    var$use_linear <- FALSE
    var$use_UD <- TRUE
    var$use_OD <- FALSE

    vars_info[[i]] <- var
  }
  for (i in qual_OD) {
    var <- vars_info[[i]]
    var$type <- "qual"

    var$use_linear <- FALSE
    var$use_UD <- FALSE
    var$use_OD <- TRUE

    vars_info[[i]] <- var
  }
  for (i in qual_both) {
    var <- vars_info[[i]]
    var$type <- "qual"

    var$use_linear <- FALSE
    var$use_UD <- TRUE
    var$use_OD <- TRUE

    vars_info[[i]] <- var
  }
  for (i in quan) {
    var <- vars_info[[i]]
    var$type <- "quan"

    var$use_linear <- add_linear_columns
    var$use_UD <- FALSE
    var$use_OD <- TRUE

    vars_info[[i]] <- var
  }


  # Append additional informations into vars_info
  for (i in seq(nvar)) {
    # For quantitative variables, holds numeric data and informations for standardization
    if (vars_info[[i]]$type == "quan") {
      data_idx <- vars_info[[i]]$data_column_idx
      x[, data_idx] <- as.numeric(x[, data_idx])
      vars_info[[i]]$mu <- mean(x[, data_idx])
      vars_info[[i]]$sigma <- sd(x[, data_idx])
    }

    # For qualitative variables, holds ordered or unordered factor data
    if (vars_info[[i]]$type == "qual") {
      data_idx <- vars_info[[i]]$data_column_idx
      if (vars_info[[i]]$use_OD) {
        x[, data_idx] <- ordered(x[, data_idx])
      } else {
        x[, data_idx] <- factor(x[, data_idx])
      }
    }

    # For variables using dummies, holds informations of the way dummies are generated
    if (vars_info[[i]]$use_UD) {
      vars_info[[i]]$UD_info <- getUDummyMatForOneVec(x[, i], only_info=TRUE)
    }
    if (vars_info[[i]]$use_OD) {
      vars_info[[i]]$OD_info <- getODummyMatForOneVec(x[, i], only_info=TRUE)
    }
  }


  # Append informations of interaction effects
  if (append_interaction_vars & nvar > 1) {
    idx <- length(vars_info)
    for (i in 1:(nvar - 1)) {
      for (j in (i + 1):nvar) {
        idx <- idx + 1
        var_info1 <- vars_info[[i]]
        var_info2 <- vars_info[[j]]
        var_info <- list(idx=idx,
                         name=paste0(var_info1$name, "_x_", var_info2$name),
                         type="inter",
                         var_idx1=i,
                         var_idx2=j)
        vars_info[[idx]] <- var_info
      }
    }
  }

  new("AGLM_Input", vars_info=vars_info, data=x)
}
