library(glmnet)

#define predictor and response variables
y <- mtcars$hp
x <- data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])

#fit ridge regression model
model <- glmnet(x, y, alpha = 0)
summary(model)

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 0)
plot(cv_model)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda

#produce Ridge trace plot
plot(model, xvar = "lambda")

#find coefficients of best model
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model)

#calculate R-squared of model on training data
y_predicted <- predict(model, s = best_lambda, newx = x)

#find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

#find R-Squared
rsq <- 1 - sse / sst
rsq