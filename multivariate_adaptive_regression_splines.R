library(ISLR)    #contains Wage dataset
library(dplyr)   #data wrangling
library(ggplot2) #plotting 
library(earth)   #fitting MARS models
library(caret)   #tuning model parameters

#create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 50, length.out = 10) %>% floor()
)

#make this example reproducible
set.seed(1)

#fit MARS model using k-fold cross-validation
cv_mars <- train(
  x = subset(Wage, select = -c(wage, logwage)),
  y = Wage$wage,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)


#display model with lowest test RMSE
cv_mars$results %>%
  filter(nprune == cv_mars$bestTune$nprune, degree == cv_mars$bestTune$degree)

#display test RMSE by terms and degree
ggplot(cv_mars)