library(MASS) # For Boston
library(aglm)

## Read data
xy <- Boston # xy is a data.frame to be processed.
colnames(xy)[ncol(xy)] <- "y" # Let medv be the objective variable, y.

## Split data into train and test
n <- nrow(xy) # Sample size.
x <- xy[-ncol(xy)]
y <- xy$y

## Plots coefs of a model trained with single lambda.
model <- aglm(x, y, lambda=0.5)
plot(model)

## Plots coefs of a model trained with multiple lambda, specifying a single lambda to be plotted.
model <- aglm(x, y)
plot(model, s=0.5)

## Plots coefs of cross-validated model
cv.model <- cv.aglm(x, y)
plot(cv.model, s=cv.model@lambda.min)

## Plots coefs of specified variables only
plot(cv.model, s=cv.model@lambda.min, vars=c(1, 2))
plot(cv.model, s=cv.model@lambda.min, vars=c("rm", "nox"))
