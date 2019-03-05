library(MASS) # For Boston
library(aglm)

## Read data
xy <- Boston # xy is a data.frame to be processed.
colnames(xy)[ncol(xy)] <- "y" # Let medv be the objective variable, y.

## Split data into train and test
n <- nrow(xy) # Sample size.
set.seed(2018) # For reproducibility.
test.id <- sample(n, round(n/4)) # ID numbders for test data.
test <- xy[test.id,] # test is the data.frame for testing.
train <- xy[-test.id,] # train is the data.frame for training.
x <- train[-ncol(xy)]
y <- train$y
newx <- test[-ncol(xy)]
y_true <- test$y

## Select the best lambda
lambda.min <- cv.aglm(x, y)@lambda.min
cat("lambda.min: ", lambda.min, "\n")

## Predict y for newx
model <- aglm(x, y, lambda=lambda.min)
y_pred <- predict(model, newx=newx)
cat("RMSE: ", sqrt(mean((y_true - y_pred)^2)), "\n")
plot(y_true, y_pred)
