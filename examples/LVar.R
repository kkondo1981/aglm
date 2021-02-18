library(aglm)
library(MASS) # For Boston

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

## Sample data for extrapolation parameter
SEED <- 2021
sd <- 0.2
set.seed(SEED)
x <- 2 * runif(1000) + 1
f <- function(x){x^3 - 6 * x^2 + 12 * x}
y <- f(x) + rnorm(1000, sd = sd)
xy <- data.frame(x=x, y=y)
x_test <- seq(0.75, 3.25, length.out=101)
y_test <- f(x_test) + rnorm(101, sd=sd)
xy_test <- data.frame(x=x_test, y=y_test)

## Sample plot for extrapolation="default"
set.seed(SEED)
model <- cv.aglm(x, y, use_LVar=TRUE)
pred <- predict(model, newx=x_test, s=model@lambda.min, type="response")
plot(x_test, y_test, pch=20, main="extrapolation='default'")
lines(x_test, pred, col="red")
lines(x_test, f(x_test))
plot(model, s=model@lambda.min, layout=c(1, 1), main="extrapolation='default'")

## Sample plot for extrapolation="flat"
set.seed(SEED)
model <- cv.aglm(x, y, use_LVar=TRUE, extrapolation="flat")
pred <- predict(model, newx=x_test, s=model@lambda.min, type="response")
plot(x_test, y_test, pch=20, main="extrapolation='flat'")
lines(x_test, pred, col="red")
lines(x_test, f(x_test))
plot(model, s=model@lambda.min, layout=c(1, 1), main="extrapolation='flat'")
