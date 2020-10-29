#DEFINE DATA
data <- mtcars[ , c("mpg", "disp", "hp", "drat")]

#view first six rows of data frame
head(data)

#EXAMINE DATA
#scatterplot to view relationships/correlations between variables
pairs(data, pch = 18, col = "steelblue")

#FIT THE MODEL
model <- lm(mpg ~ disp + hp + drat, data = data)

#CHECK MODEL ASSUMPTIONS
#create histogram to check for residual normality
hist(residuals(model), col = "steelblue")

#create fitted value vs residual plot for heteroskedasticity
plot(fitted(model), residuals(model))
abline(h = 0, lty = 2)

#VIEW MODEL SUMMARY
summary(model)

#USE MODEL TO MAKE PREDICTIONS
#define the coefficients from the model output
intercept <- coef(summary(model))["(Intercept)", "Estimate"]
disp <- coef(summary(model))["disp", "Estimate"]
hp <- coef(summary(model))["hp", "Estimate"]
drat <- coef(summary(model))["drat", "Estimate"]

#use the model coefficients to predict the value for mpg
intercept + disp*220 + hp*150 + drat*3