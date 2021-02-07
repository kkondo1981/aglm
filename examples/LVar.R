library(aglm)
library(MASS) # For Boston
library(Metrics)

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

##
## Comparison between extrapolation_flat(FALSE/TRUE), OD and GAM
##

SEED <- 2021
sd <- 0.2
set.seed(SEED)
x <- 2 * runif(1000) + 1
f <- function(x){x ^ 3 - 6 * x ^ 2 + 12 * x}
y <- f(x) + rnorm(1000, sd = sd)
#plot(x, y)
xy <- as.data.frame(cbind(x = x, y = y))
x_test <- seq(0.75, 3.25, length.out = 101)
y_test <- f(x_test) + rnorm(101, sd = sd)
xy_test <- as.data.frame(cbind(x = x_test, y = y_test))

set.seed(SEED)
model_default <- model <- cv.aglm(x, y, use_LVar = TRUE)
pred <- predict(model, newx = x_test,
                s = model@lambda.min,
                type = "response")
(rmse_default <- Metrics::rmse(y_test, pred))
plot(x_test, y_test, pch = 20)
lines(x_test, pred, col = "red")
lines(x_test, f(x_test))
plot(model, s = model@lambda.min, layout = c(1, 1))

set.seed(SEED)
model_flat <- model <-
  cv.aglm(x, y, use_LVar = TRUE, LVar_extrapolation_flat = TRUE)
pred <- predict(model, newx = x_test,
                s = model@lambda.min,
                type = "response")
(rmse_flat <- Metrics::rmse(y_test, pred))
plot(x_test, y_test, pch = 20)
lines(x_test, pred, col = "red")
lines(x_test, f(x_test))
plot(model, s = model@lambda.min, layout = c(1, 1))

set.seed(SEED)
model_OD <- model <- cv.aglm(x, y)
pred <- predict(model, newx = x_test,
                s = model@lambda.min,
                type = "response")
(rmse_OD <- Metrics::rmse(y_test, pred))
plot(x_test, y_test, pch = 20)
lines(x_test, pred, col = "red")
lines(x_test, f(x_test))
plot(model, s = model@lambda.min, layout = c(1, 1))

model_gam <- model <- mgcv::gam(y ~ s(x), data = xy)
pred <- predict(model, newdata = xy_test, type = "response")
(rmse_gam <- Metrics::rmse(y_test, pred))
plot(x_test, y_test, pch = 20)
lines(x_test, pred, col = "red")
lines(x_test, f(x_test))
plot(model)

c(rmse_default = rmse_default,
  rmse_flat = rmse_flat,
  rmse_OD = rmse_OD,
  rmse_gam = rmse_gam)

