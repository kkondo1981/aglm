library(MASS) # For Boston
library(aglm)

## Read data
xy <- Boston # xy is a data.frame to be processed.
colnames(xy)[ncol(xy)] <- "y" # Let medv be the objective variable, y.

## Split data into train and test
n <- nrow(xy) # Sample size.
x <- xy[-ncol(xy)]
y <- xy$y

## Select the best lambda by `cv.aglm()`, fixing `alpha=1` (LASSO)
cv.model <- cv.aglm(x, y, use_LVar=TRUE)
lambda.min <- cv.model@lambda.min
cat("lambda.min: ", lambda.min, "\n")

## Plots coefs of cross-validated model
plot(cv.model, s=cv.model@lambda.min, resid=TRUE, add_rug=TRUE)
