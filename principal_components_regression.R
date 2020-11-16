library(pls)

#make this example reproducible
set.seed(1)

#fit PCR model
model <- pcr(hp~mpg+disp+drat+wt+qsec, data=mtcars, scale=TRUE, validation="CV")

#view summary of model fitting
summary(model)

#visualize CV plots
validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

#define training and testing sets
train <- mtcars[1:25, c("hp", "mpg", "disp", "drat", "wt", "qsec")]
y_test <- mtcars[26:nrow(mtcars), c("hp")]
test <- mtcars[26:nrow(mtcars), c("mpg", "disp", "drat", "wt", "qsec")]
    
#use model to make predictions on a test set
model <- pcr(hp~mpg+disp+drat+wt+qsec, data = train, scale =TRUE, validation = "CV")
pcr_pred <- predict(model, test, ncomp = 2)

#calculate RMSE
sqrt(mean((pcr_pred - y_test)^2))