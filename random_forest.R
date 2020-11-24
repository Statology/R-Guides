#load necessary libraries
library(randomForest)

#view first six rows of airquality dataset
head(airquality)

#find number of rows with missing values
sum(!complete.cases(airquality))

#replace NAs with column medians
for(i in 1:ncol(airquality)) {
  airquality[ , i][is.na(airquality[ , i])] <- median(airquality[ , i], na.rm=TRUE)
}

#make this example reproducible
set.seed(1)

#fit random forest model
model <- randomForest(
  formula = Ozone ~ .,
  data = airquality
)

#display fitted model
model

#display test MSE by number of trees
plot(model)

#find number of trees that produce lowest test MSE
which.min(model$mse)

#find RMSE of best model
sqrt(model$mse[which.min(model$mse)])

#produce variable importance plot
varImpPlot(model) 

#tune model
model_tuned <- tuneRF(
               x=airquality[,-1],
               y=airquality$Ozone,
               ntreeTry=500,
               mtryStart=4,
               stepFactor=1.5,
               improve=0.01,
               trace=FALSE #don't show real-time progress
               )
			   
#define new observation
new <- data.frame(Solar.R=150, Wind=8, Temp=70, Month=5, Day=5)

#use fitted random forest model to predict Ozone value of new observation
predict(model, newdata=new)