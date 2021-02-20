library(MASS) # For Boston
library(aglm)

## Randomly created train and test data
set.seed(2021)
sd <- 0.2
x <- 2 * runif(1000) + 1
f <- function(x){x^3 - 6 * x^2 + 13 * x}
y <- f(x) + rnorm(1000, sd = sd)
xy <- data.frame(x=x, y=y)
x_test <- seq(0.75, 3.25, length.out=101)
y_test <- f(x_test) + rnorm(101, sd=sd)
xy_test <- data.frame(x=x_test, y=y_test)

## Sample plot for extrapolation="default"
models <- c(cv.aglm(x, y, extrapolation="default"),
            cv.aglm(x, y, extrapolation="flat"),
            cv.aglm(x, y, use_LVar=TRUE, extrapolation="default"),
            cv.aglm(x, y, use_LVar=TRUE, extrapolation="flat"))

titles <- c("O-Dummies with extrapolation=\"default\"",
            "O-Dummies with extrapolation=\"flat\"",
            "L-Variables with extrapolation=\"default\"",
            "L-Variables with extrapolation=\"flat\"")

par.old <- par(mfrow=c(2, 2))
for (i in 1:4) {
  model <- models[[i]]
  title <- titles[[i]]

  pred <- predict(model, newx=x_test, s=model@lambda.min, type="response")

  plot(x_test, y_test, pch=20, col="grey", main=title)
  lines(x_test, f(x_test), lty="dashed", lwd=2)  # the theoretical line
  lines(x_test, pred, col="blue", lwd=3)  # the smoothed line by the model
}
par(par.old)
