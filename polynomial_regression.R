library(ggplot2)

set.seed(1)

#create dataset
df <- data.frame(hours = runif(50, 5, 15), score=50)
df$score = df$score + df$hours^3/150 + df$hours*runif(50, 1, 2)

#view first six rows of data
head(df)

#create scatterplot of data
ggplot(df, aes(x=hours, y=score)) +
  geom_point()

#randomly shuffle data
df.shuffled <- df[sample(nrow(df)),]

#define number of folds to use for k-fold cross-validation
K <- 10 

#define degree of polynomials to fit
degree <- 5

#create k equal-sized folds
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)

#create object to hold MSE's of models
mse = matrix(data=NA,nrow=K,ncol=degree)

#Perform K-fold cross validation
for(i in 1:K){
    
    #define training and testing data
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- df.shuffled[testIndexes, ]
    trainData <- df.shuffled[-testIndexes, ]
    
    #use k-fold cv to evaluate models
    for (j in 1:degree){
        fit.train = lm(score ~ poly(hours,j), data=trainData)
        fit.test = predict(fit.train, newdata=testData)
        mse[i,j] = mean((fit.test- testData$score)^2) 
    }
}

#find MSE for each degree 
colMeans(mse)

#obtain coefficients of best model
best = lm(score ~ poly(hours,2, raw=T), data=df)
summary(best)

#plot final fitted model
ggplot(df, aes(x=hours, y=score)) + 
          geom_point() +
          stat_smooth(method='lm', formula = y ~ poly(x,2), size = 1) + 
          xlab('Hours Studied') +
          ylab('Score')