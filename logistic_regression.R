#LOAD DATA

#load dataset
data <- ISLR::Default

#view summary of dataset & total observations
summary(data)
nrow(data)

#CREAT TRAINING AND TESTING SAMPLES
#make this example reproducible
set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]

#FIT THE LOGISTIC REGRESSION MODEL
model <- glm(default~student+balance+income, family="binomial", data=train)

#disable scientific notation for model summary
options(scipen=999)

#view model summary
summary(model)

#calculate McFadden's R-Squared
pscl::pR2(model)["McFadden"]

#calculate variable importance
caret:varImp(model)

#calculate VIF values
car::vif(model)

#USE MODEL TO MAKE PREDICTIONS
predicted <- predict(model, test, type="response")


#MODEL DIAGNOSTICS
library(InformationValue)

#convert defaults from "Yes" and "No" to 1's and 0's
test$default <- ifelse(test$default=="Yes", 1, 0)

#find optimal cutoff probability to use to maximize accuracy
optimal <- optimalCutoff(test$default, predicted)[1]
optimal

#create confusion matrix
confusionMatrix(test$default, predicted)

#calculate sensitivity
sensitivity(test$default, predicted)

#calculate specificity
specificity(test$default, predicted)

#calculate total misclassification error rate
misClassError(test$default, predicted, threshold=optimal)

#plot the ROC curve
plotROC(test$default, predicted)