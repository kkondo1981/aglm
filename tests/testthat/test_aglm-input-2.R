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

## Function to produce a data.frame of two-way interactions
ints.mat <- function(data){
  ints <- NULL
  temp <- colnames(data)
  for(i in 1:(ncol(data)-1)){
    for(j in (i+1):(ncol(data))){
      ints <- cbind(ints, data[, i] * data[, j])
      colnames(ints)[ncol(ints)] <- paste0(temp[i], ".", temp[j])
    }
  }
  ints
}

# Sort columns of a matrix by ascending order of column sums, to compare matrices ignoring column orders
sort.columns <- function(x) {
  sums <- apply(x, FUN=sum, MARGIN=2)
  return(x[, order(sums)])
}


# Tests
test_that("Check design matrix for actual data", {
  x <- Boston[-ncol(Boston)]
  DM.aglm <- sort.columns(getDesignMatrix(newInput(x)))
  DM.Iwasawa <- sort.columns(cbind(OD(x, make.bins(x)), as.matrix(x), ints.mat(x)))

  expect_equal(as.numeric(DM.aglm), as.numeric(DM.Iwasawa))
})
