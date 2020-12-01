library(tidyverse)

#load data
data("USArrests")

#view first six rows of data
head(USArrests)

#calculate principal components
results <- prcomp(USArrests, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

#reverse the signs of the scores
results$x <- -1*results$x

#display the first six scores
head(results$x)

#create biplot to visualize results
biplot(results, scale = 0)

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:4), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)