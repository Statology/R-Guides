library(xgboost) #for fitting the xgboost model
library(caret) #for general data preparation and model fitting

#load the data
data = MASS::Boston

#view the structure of the data
str(data)

#make this example reproducible
set.seed(0)

#split into training (80%) and testing set (20%)
parts = createDataPartition(data$medv, p = .8, list = F)
train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(train[, -13])
train_y = train[,13]

#define predictor and response variables in testing set
test_x = data.matrix(test[, -13])
test_y = test[, 13]

#fit XGBoost model to training set
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#define watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each round
model = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)

#define final model
final = xgboost(data = xgb_train, max.depth = 3, nrounds = 56, verbose = 0)

#use model to make predictions on test data
pred_y = predict(final, xgb_test)

#measure prediction accuracy
mean((test_y - pred_y)^2) #mse
caret::MAE(test_y, pred_y) #mae
caret::RMSE(test_y, pred_y) #rmse