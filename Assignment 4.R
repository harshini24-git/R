library(glmnet)
library(caret)
library(ISLR)
attach(College)
head(College)
#Splitting the Data into Test and Train sets
set.seed(3456)
trainIndex <- createDataPartition(College$Private, p = .8,
                                  list = FALSE,
                                  times = 1)
Train <- College[ trainIndex,]
Test <- College[-trainIndex,]

# Compute RMSE 
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - (SSE / SST)
  RMSE = sqrt(SSE/nrow(df))
  
  
  # Model performance metrics
  data.frame(
    RMSE = RMSE
  )
  
}
#model evaluation

train_y <-factor(ifelse(College$Private == "Yes",1,0))

train_x <- model.matrix(College[,-1])
lambdas <- 10^seq(3, -2, by = -.1)
cv.ridgefit <- cv.glmnet(train_x,train_y, alpha = 0, lambda = lambdas, family = "binomial")
plot(cv.ridgefit)
cv.ridgefit$lambda.min
cv.ridgefit$lambda.1se
Ridgefit.min <- glmnet(train_x, train_y, alpha = 0, family = "binomial", lambda = cv.ridge$lambda.min, standardize = TRUE)
coef(Ridgefit.min )
Ridgefit.1se <- glmnet(train_x, train_y, alpha = 0, family = "binomial", lambda = cv.ridge$lambda.1se, standardize = TRUE)
coef(Ridgefit.1se )
#module prediction
x_train = data.matrix(Train[,-1])
y_train = Train$Private
x_test= data.matrix(Test[,-1])
y_test = Test$Private
#Ridge Test model
train_model <- predict(cv.ridgefit, newx = x_train, type = "response")
predtrain<-factor(ifelse(train_model>0.5,1,0))
#Ridge Train Model
test_model <- predict(cv.ridgefit, newx = x_test, type = "response")
predtest <- factor(ifelse(test_model>0.5,1,0))
# Rmse on train data
eval_results(as.numeric(y_train), as.numeric(train_model), Train)
# Rmse on test data
eval_results(as.numeric(y_test),as.numeric(test_model), Test)
#model accuracy
#train
confusionMatrix( y_train,  predtrain)
#test
confusionMatrix(y_test, predtest)
###############   Lasso Regression    ###########################
lassofit <- cv.glmnet(train_x,train_y, alpha = 1, lambda = lambdas, family = "binomial")
summary(lassofit)
plot(lassofit)
lassofit$lambda.min
lassofit$lambda.1se
lassofit.min <- glmnet(train_x, train_y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.min)
coef(lassofit.min )
lassofit.1se <- glmnet(train_x, train_y, alpha = 1, family = "binomial", lambda = cv.lasso$lambda.1se)
coef(lassofit.1se )
#lasso Train model
train_modell <- predict(lassofit, newx = x_train, type = "response")
predtrainn<-factor(ifelse(train_modell>0.5,1,0))
#lasso Test model
test_modell <- predict(lassofit, newx = x_test, type = "response")
predtestt <- factor(ifelse(test_modell>0.5,1,0))
#Rmse on train data
eval_results(as.numeric(y_train), as.numeric(train_modell), Train)
#Rmse on test data
eval_results(as.numeric(y_test),as.numeric(test_modell), Test)
#model accuracy
#train
confusionMatrix(y_test, predtestt )
#test
confusionMatrix(y_train, predtrainn )




