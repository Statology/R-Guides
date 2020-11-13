library(glmnet)

#Define predictor and response variables
y <- mtcars$hp
x <- data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])

#fit lasso regression model using k-fold cross-validation
cv_model <- cv.glmnet(x, y, alpha = 1)
best_lambda <- cv_model$lambda.min

#display optimal lambda value
best_lambda

#view plot of test MSE's vs. lambda values
plot(cv_model)

#view coefficients of best model
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

#make a prediction for the response value of a new observation
new = matrix(c(24, 2.5, 3.5, 18.5), nrow=1, ncol=4) 
predict(best_model, s = best_lambda, newx = new)

#find R-squared of model on training data
y_predicted <- predict(best_model, s = best_lambda, newx = x)

sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

rsq <- 1 - sse/sst
rsq