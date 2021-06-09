
#################### Cross-validation for alpha and lambda ####################

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

# NOTE: Codes bellow will take considerable time, so run it when you have time.
\donttest{

## Fit the model
cva_result <- cva.aglm(x, y, family="binomial")

alpha <- cva_result@alpha.min
lambda <- cva_result@lambda.min

mod_idx <- cva_result@alpha.min.index
model <- cva_result@models_list[[mod_idx]]

## Make the confusion matrix
y_true <- test$vote
y_pred <- levels(y_true)[as.integer(predict(model, newx, s=lambda, type="class"))]

cat(sprintf("Confusion matrix for alpha=%.5f and lambda=%.5f:\n", alpha, lambda))
print(table(y_true, y_pred))

}
