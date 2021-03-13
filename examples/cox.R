library(aglm)
library(survival)
data("kidney")

x <- kidney[, 4:7] #"age", "sex", "disease", "frail"
y <- Surv(kidney$time, kidney$status)
model <- cv.aglm(x, y, family="cox")
plot(model, s=model@lambda.min)
