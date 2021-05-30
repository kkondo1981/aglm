
#################### Cross-validation for lambda ####################

library(aglm)
library(faraway)

## Read data
xy <- nes96

## Split data into train and test
n <- nrow(xy) # Sample size.
set.seed(2018) # For reproducibility.
test.id <- sample(n, round(n/5)) # ID numbders for test data.
test <- xy[test.id,] # test is the data.frame for testing.
train <- xy[-test.id,] # train is the data.frame for training.
x <- train[, c("popul", "TVnews", "selfLR", "ClinLR", "DoleLR", "PID", "age", "educ", "income")]
y <- train$vote
newx <- test[, c("popul", "TVnews", "selfLR", "ClinLR", "DoleLR", "PID", "age", "educ", "income")]

## Fit the model
model <- cv.aglm(x, y, family="binomial")

## Make the confusion matrix
lambda <- model@lambda.min
y_true <- test$vote
y_pred <- levels(y_true)[as.integer(predict(model, newx, s=lambda, type="class"))]

cat(sprintf("Confusion matrix for lambda=%.5f:\n", lambda))
print(table(y_true, y_pred))
