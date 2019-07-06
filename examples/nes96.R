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
x.new <- test[, c("popul", "TVnews", "selfLR", "ClinLR", "DoleLR", "PID", "age", "educ", "income")]
y.true <- test$vote

## Fitting, Prediction with Cross Validation of lambda
cat("Prediction with cross validation of lambda:\n")
model.cv <- cv.aglm(x, y, family="binomial", add_interaction_columns=FALSE)
lambda.min <- model.cv@lambda.min
cat("lambda.min: ", lambda.min, "\n")
plot(model.cv, s=model.cv@lambda.min, verbose=FALSE)
y.pred.cv <- predict(model.cv, x.new, type="class")
print(table(y.true, y.pred.cv[, length(y.pred.cv[1, ])]))

## Fitting, Prediction with Cross Validation of both lambda and alpha
cat("\n\nPrediction with cross validation of both lambda and alpha:\n")
model.cva <- cva.aglm(x, y, add_interaction_columns=FALSE)
lambda.min <- model.cva@lambda.min
cat("lambda.min: ", lambda.min, "\n")
alpha.min <- model.cva@alpha.min
cat("alpha.min: ", alpha.min, "\n")
plot(model.cva@models_list[[model.cva@alpha.min.index]], s=model.cva@lambda.min, verbose=FALSE)

model.best <- aglm(x, y, family="binomial", lambda=lambda.min, alpha=alpha.min,
                   add_interaction_columns=FALSE)
y.pred.best <- predict(model.best, x.new, type="class")
print(table(y.true, y.pred.best[, length(y.pred.best[1, ])]))
