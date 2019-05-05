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
newInput <- function(x,
                     qualitative_vars_UD_only=NULL,
                     qualitative_vars_both=NULL,
                     qualitative_vars_OD_only=NULL,
                     quantitative_vars=NULL,
                     add_linear_columns=TRUE,
                     add_OD_columns_of_qualitatives=TRUE,
                     add_interaction_columns=TRUE,
                     OD_type_of_quantitatives='C',
                     bins_list=NULL,
                     bins_names=NULL) {
  # Check and process arguments
  assert_that(!is.null(x))
  if (class(x) != "data.frame") x <- data.frame(x)
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

    var$type <- if (is.factor(x[, i]) | is.logical(x[, i])) {"qual"} else {"quan"}
    is_ordered <- (var$type == "qual" & is.ordered(x[, i])) | (var$type == "quan")

    var$use_linear <- var$type == "quan" & add_linear_columns
    var$use_UD <- var$type == "qual"
    var$use_OD <- var$type == "quan" | (var$type == "qual" & is_ordered & add_OD_columns_of_qualitatives)
    if (var$use_OD) {
      if (var$type == "quan") var$OD_type <- OD_type_of_quantitatives
      else var$OD_type <- 'J'
    }

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
  assert_that(length(intersect(qual_UD, qual_OD)) == 0, msg=msg)
  assert_that(length(intersect(qual_UD, qual_both)) == 0, msg=msg)
  assert_that(length(intersect(qual_UD, quan)) == 0, msg=msg)
  assert_that(length(intersect(qual_OD, qual_both)) == 0, msg=msg)
  assert_that(length(intersect(qual_OD, quan)) == 0, msg=msg)
  assert_that(length(intersect(qual_both, quan)) == 0, msg=msg)


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


  # Retypes columns
  for (i in seq(nvar)) {
    # For quantitative variables, holds numeric data
    if (vars_info[[i]]$type == "quan") {
      data_idx <- vars_info[[i]]$data_column_idx
      x[, data_idx] <- as.numeric(x[, data_idx])
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
  }

  # Set binning informations from bins_list
  if (!is.null(bins_list)) {
    if (is.null(bins_names)) {
      idx_list <- list()
      for (i in seq(nvar)) {
        v <- vars_info[[i]]
        if (v$use_OD) idx_list <- c(idx_list, v$idx)
      }

      for (i in seq(length(bins_list))) {
        idx <- idx_list[[i]]
        breaks <- bins_list[[i]]
        vars_info[[idx]]$OD_info$breaks <- unique(sort(breaks[is.finite(breaks)]))
      }
    } else {
      idx_map <- list()
      if (all(sapply(bins_names, is.character))) {
        for (i in seq(nvar)) {
          v <- vars_info[[i]]
          if (v$use_OD) idx_map[[v$name]] <- v$idx
        }
      } else {
        for (i in seq(nvar)) {
          v <- vars_info[[i]]
          if (v$use_OD) idx_map[[v$idx]] <- v$idx
        }
      }

      for (i in seq(length(bins_list))) {
        name <- bins_names[[i]]
        idx <- idx_map[[name]]
        vars_info[[idx]]$OD_info$breaks <- bins_list[[i]]
      }
    }
  }

  # For variables using dummies, set informations of the way dummies are generated
  for (i in seq(nvar)) {
    if (vars_info[[i]]$use_UD) {
      vars_info[[i]]$UD_info <- getUDummyMatForOneVec(x[, i], only_info=TRUE, drop_last=FALSE)
    }
    if (vars_info[[i]]$use_OD & is.null(vars_info[[i]]$OD_info)) {
      vars_info[[i]]$OD_info <- getODummyMatForOneVec(x[, i], dummy_type=vars_info[[i]]$OD_type, only_info=TRUE)
    }
  }


  # Append informations of interaction effects
  if (add_interaction_columns & nvar > 1) {
    idx <- length(vars_info)
    for (i in 1:nvar) {
      for (j in i:nvar) {
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


# Functions for inner use
getMatrixRepresentation <- function(x, idx, drop_OD=FALSE) {
  var_info <- x@vars_info[[idx]]
  z <- NULL

  if (var_info$type == "quan" | var_info$type == "qual") {
    z_raw <- x@data[, var_info$data_column_idx]

    if (var_info$use_linear) {
      z <- matrix(z_raw, ncol=1)
      colnames(z) <- var_info$name
    }

    if (var_info$use_OD & !drop_OD) {
      z_OD <- getODummyMatForOneVec(z_raw, breaks=var_info$OD_info$breaks, dummy_type=var_info$OD_type)$dummy_mat
      colnames(z_OD) <- paste0(var_info$name, "_OD_", seq(dim(z_OD)[2]))
      z <- cbind(z, z_OD)
    }

    if (var_info$use_UD) {
      z_UD <- getUDummyMatForOneVec(z_raw, levels=var_info$UD_info$levels,
                                    drop_last=var_info$UD_info$drop_last)$dummy_mat
      colnames(z_UD) <- paste0(var_info$name, "_UD_", seq(dim(z_UD)[2]))
      z <- cbind(z, z_UD)
    }
  } else if (var_info$type == "inter") {
    # Interaction effects between columns of one variable itself
    self_interaction <- var_info$var_idx1 == var_info$var_idx2

    # Get matrix representations of two variables
    z1 <- getMatrixRepresentation(x, var_info$var_idx1, drop_OD=TRUE)
    z2 <- getMatrixRepresentation(x, var_info$var_idx2, drop_OD=TRUE)
    if (is.null(z1) | is.null(z2))
      return(NULL)

    # Create matrix representation of intarction
    nrow <- dim(z1)[1]
    ncol1 <- dim(z1)[2]
    ncol2 <- dim(z2)[2]
    ncol_res <- ifelse(self_interaction, ncol1 * (ncol1 - 1) / 2, ncol1 * ncol2)
    assert_that(dim(z2)[1] == nrow)
    z <- matrix(0, nrow, ncol_res)
    nm <- character(ncol_res)
    ij <- 0
    for (i in 1:ncol1) {
      js <- 1:ncol2
      if (self_interaction)
        js <- js[js > i]
      for (j in js) {
        ij <- ij + 1
        z[, ij] <- z1[, i] * z2[, j]
        nm[ij] <- paste0(var_info$name, "_", i, "_", j)
      }
    }
    colnames(z) <- nm
  } else {
    assert_true(FALSE)  # never expects to come here
  }

  return(z)
}


#' Get design-matrix representation of AGLM_Input objects
#'
#' @param x An AGLM_Input object
#'
#' @return A data.frame which represents the matrix representation of `x`.
#'
#' @export
#' @importFrom assertthat assert_that
getDesignMatrix <- function(x) {
  # Check arguments
  assert_that(class(x) == "AGLM_Input")

  # Data size
  nobs <- dim(x@data)[1]
  nvar <- length(x@vars_info)
  x_mat <- NULL

  for (i in 1:nvar) {
    z <- getMatrixRepresentation(x, i)
    if (i == 1) x_mat <- z
    else x_mat <- cbind(x_mat, z)
  }

  return(x_mat)
}
