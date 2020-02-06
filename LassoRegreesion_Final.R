library(plyr)
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(repr)
library(glmnet)

library(readr)

#Loss function = OLS + alpha * summation (absolute values of the magnitude of the coefficients


#Loss function = OLS + alpha * summation (squared coefficient values)

#nlambda: determines the number of regularization parameters to be tested.

#alpha: determines the weighting to be used. In case of ridge regression, the value of alpha is zero.

#family: determines the distribution family to be used. Since this is a regression model, we will use the Gaussian distribution.

#lambda: determines the lambda values to be tried.


CarsData <- read_csv(file.choose())

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



# Setting alpha = 1 implements lasso regression
lasso_reg <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambdas, standardize = TRUE, nfolds = 5)

lambda_best <- lasso_reg$lambda.min 
lambda_best


lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_best, standardize = TRUE)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x_train)
eval_results(y_train, predictions_train, train)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
eval_results(y_test, predictions_test, test)



summary(lasso_model)

coef(lasso_model)

plot(lasso_reg)

















