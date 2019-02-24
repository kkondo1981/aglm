context("aglm-input-2")
library(MASS) # For Boston.
library(aglm)

# Check design matrices for actual data are same as those produced by Iwasawa-san's original codes
# Utility functions written by Iwasawa-san for oridinal AGLM scripts

# Function to produce a data.frame of O-dummies
make.bins <- function(data, max.nbin = 100){
  temp <- apply(data, 2, function(x){as.vector(quantile(x, seq(0, 1, 1 / (min(max.nbin, length(x)) - 1))))})
  apply(temp, 2, unique)
}

OD <- function(data, bins){
  x.OD <- NULL
  for (i in 1:length(bins)){
    n <- nrow(data)
    m <- length(bins[[i]])
    for(j in 1:m){
      temp <- data[,i]
      for(k in 1:n){
        temp[k] <- ifelse(temp[k] < bins[[i]][j], 0, 1)
      }
      x.OD <- cbind(x.OD, temp)
      colnames(x.OD)[ncol(x.OD)] <- paste0(colnames(data)[i], j)
    }
  }
  x.OD
}

UD <- function(x, name){
  x.UD <- NULL
  for(i in 1:(nlevels(x))){
    lev <- levels(x)[i]
    temp <- sapply(x, FUN = function(x){ifelse(x == lev, 1, 0)})
    x.UD <- cbind(x.UD, temp)
    colnames(x.UD)[i] <- paste0(name, levels(x)[i])
  }
  x.UD
}

## Function to produce a data.frame of two-way interactions
ints.mat <- function(data){
  ints <- NULL
  temp <- colnames(data)
  for(i in 1:(ncol(data)-1)){
    for(j in (i+1):ncol(data)){
      ints <- cbind(ints, data[, i] * data[, j])
      colnames(ints)[ncol(ints)] <- paste0(temp[i], ".", temp[j])
    }
  }
  ints
}

# To compare two matrices allowing column-reordering, we use checksums.
compare_mat_without_column_order <- function(x, y) {
  if (!all(dim(x) == dim(y)))
    return(FALSE)

  # Row-wise equality test. Orders are considered.
  if (!all(apply(x, FUN=sum, MARGIN=1) == apply(x, FUN=sum, MARGIN=1)))
    return(FALSE)

  # Column-wise equality test. Orders are not considered.
  if (!all(sort(apply(x, FUN=sum, MARGIN=2)) == sort(apply(x, FUN=sum, MARGIN=2))))
    return(FALSE)

  return(TRUE)
}

# Tests
test_that("Check design matrix for actual data 1", {
  ## Read data
  x <- Boston[-ncol(Boston)]

  ## Create design matrix of aglm
  DM.aglm <- getDesignMatrix(newInput(x))

  ## Create design matrix to be compared
  DM.Iwasawa <- cbind(OD(x, make.bins(x)), as.matrix(x), ints.mat(x))

  ## Test if two design matrice are same
  expect_true(compare_mat_without_column_order(DM.aglm, DM.Iwasawa))
})

test_that("Check design matrix for actual data 2", {
  ## Read data
  x <- Boston[, -ncol(Boston)]

  ## Create bins
  bins_list <- make.bins(x[, colnames(x) != "chas"])
  bins_names <- colnames(x)[colnames(x) != "chas"]

  ## Set chas and rad variables as factors
  x$chas <- as.factor(x$chas)
  x$rad <- as.ordered(x$rad)

  ## Create design matrix of aglm
  input.aglm <- newInput(x, bins_list=bins_list, bins_names=bins_names)
  DM.aglm <- getDesignMatrix(input.aglm)

  ## Create design matrix to be compared
  x.OD <- OD(as.matrix(x[colnames(x) != "chas"]), bins_list)
  x.UD <- cbind(UD(x$chas, "chas"), UD(x$rad, "rad"))
  x.linear <- as.matrix(x[!colnames(x) %in% c("chas", "rad")])
  x.ints <- ints.mat(cbind(x.UD, x.linear))
  DM.Iwasawa <- cbind(x.linear, x.OD, x.UD, x.ints)

  ## Test if two design matrice are same
  expect_true(compare_mat_without_column_order(DM.aglm, DM.Iwasawa))
})
