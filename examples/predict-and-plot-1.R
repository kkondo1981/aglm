
#################### using plot() and predict() ####################

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

## With the result of aglm()
model <- aglm(x, y)
lambda <- 0.1

plot(model, s=lambda, resid=TRUE, add_rug=TRUE,
     verbose=FALSE, layout=c(3, 3))

y_pred <- predict(model, newx=newx, s=lambda)
plot(y_true, y_pred)

## With the result of cv.aglm()
model <- cv.aglm(x, y)
lambda <- model@lambda.min

plot(model, s=lambda, resid=TRUE, add_rug=TRUE,
     verbose=FALSE, layout=c(3, 3))

y_pred <- predict(model, newx=newx, s=lambda)
plot(y_true, y_pred)


