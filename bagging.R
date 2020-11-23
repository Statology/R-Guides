library(dplyr)       #for data wrangling
library(ggplot2)     #for creating plots
library(e1071)       #for calculating variable importance
library(caret)       #for general model fitting
library(rpart)       #for fitting decision trees
library(ipred)       #for fitting bagged decision trees

#view structure of airquality dataset
str(airquality)

#make this example reproducible
set.seed(1)

#fit the bagged model
bag <- bagging(
  formula = Ozone ~ .,
  data = airquality,
  nbagg = 150,   
  coob = TRUE,
  control = rpart.control(minsplit = 2, cp = 0)
)

#display fitted bagged model
bag

#calculate variable importance
VI <- data.frame(var=names(airquality[,-1]), imp=varImp(bag))

#sort variable importance descending
VI_plot <- VI[order(VI$Overall, decreasing=TRUE),]

#visualize variable importance with horizontal bar plot
barplot(VI_plot$Overall,
        names.arg=rownames(VI_plot),
        horiz=TRUE,
        col='steelblue',
        xlab='Variable Importance')

#define new observation
new <- data.frame(Solar.R=150, Wind=8, Temp=70, Month=5, Day=5)

#use fitted bagged model to predict Ozone value of new observation
predict(bag, newdata=new)