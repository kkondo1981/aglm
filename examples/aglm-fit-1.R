
#################### Gaussian case ####################

library(MASS) # For Boston
library(aglm)

## Read data
xy <- Boston # xy is a data.frame to be processed.
colnames(xy)[ncol(xy)] <- "y" # Let medv be the objective variable, y.

## Split data into train and test
n <- nrow(xy) # Sample size.
set.seed(2018) # For reproducibility.
test.id <- sample(n, round(n/4)) # ID numbers for test data.
test <- xy[test.id,] # test is the data.frame for testing.
train <- xy[-test.id,] # train is the data.frame for training.
x <- train[-ncol(xy)]
y <- train$y
newx <- test[-ncol(xy)]
y_true <- test$y

## Fit the model
model <- aglm.fit(x, y)  # alpha=1 (the default value)

## Predict for various alpha and lambda
lambda <- 0.1
y_pred <- predict(model, newdata=newx, s=lambda)
rmse <- sqrt(mean((y_true - y_pred)^2))
cat(sprintf("RMSE for lambda=%.2f: %.5f \n\n", lambda, rmse))

lambda <- 1.0
y_pred <- predict(model, newdata=newx, s=lambda)
rmse <- sqrt(mean((y_true - y_pred)^2))
cat(sprintf("RMSE for lambda=%.2f: %.5f \n\n", lambda, rmse))

alpha <- 0
model <- aglm.fit(x, y, alpha=alpha)

lambda <- 0.1
y_pred <- predict(model, newdata=newx, s=lambda)
rmse <- sqrt(mean((y_true - y_pred)^2))
cat(sprintf("RMSE for alpha=%.2f and lambda=%.2f: %.5f \n\n", alpha, lambda, rmse))
