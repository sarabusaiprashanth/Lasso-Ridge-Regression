library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

library(readr)


#Loss function = OLS + alpha * summation (squared coefficient values)

#nlambda: determines the number of regularization parameters to be tested.

#alpha: determines the weighting to be used. In case of ridge regression, the value of alpha is zero.

#family: determines the distribution family to be used. Since this is a regression model, we will use the Gaussian distribution.

#lambda: determines the lambda values to be tried.


CarsData = read_csv(file.choose())
View(CarsData)




#Splitting the data into train and test
Cars_Data = sort(sample(nrow(CarsData), nrow(CarsData)*.80))

#creating training data set by selecting the output row values

train<- CarsData[Cars_Data,]

#creating test data set by not selecting the output row values
test <- CarsData[-Cars_Data,]


x_train <- as.matrix(train)
y_train <- train$MPG

x_test <- as.matrix(test)
y_test <- test$MPG


lambdas <- 10^seq(2, -3, by = -.1)

ridge_regr = glmnet(x_train, y_train, nlambda = 25, alpha = 0.4, family = 'gaussian', lambda = lambdas)

summary(ridge_regr)


cv_ridge <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambdas)
optimal_lambda <- cv_ridge$lambda.min

optimal_lambda

coef(ridge_regr)

plot(cv_ridge)


#Compute R^2 from true and predicted values
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square
  )
  
}

# Prediction and evaluation on train data
predictions_train <- predict(ridge_regr, s = optimal_lambda, newx = x_train)
eval_results(y_train, predictions_train, train)

# Prediction and evaluation on test data
predictions_test <- predict(ridge_regr, s = optimal_lambda, newx = x_test)
eval_results(y_test, predictions_test, test)




for(i in 0:10) {
  assign(paste("fit",i, sep = ""), cv.glmnet(x_train, y_train, type.measure = "mse",
                                             alpha = i/10, family = "gaussian"))
  
}


plot(cv_ridge, xvar = "lambda")
plot(fit0, main = "Ridge")


install.packages("ggridges")
library(ggridges)


plot(coef(cv_ridge))



























