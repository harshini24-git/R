library(ISLR)
attach(College)
library(caret)
library(e1071)
library(ROCR)
library(funModeling)
#Descriptive stastics
str(College)
head(College)
summary(College)
#EDA
plot_num(College)
profiling_num(College)
#Splitting the Data into Test and Train sets
set.seed(3456)
trainIndex <- createDataPartition(College$Private, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- College[ trainIndex,]
Test <- College[-trainIndex,]
#Logistic Model
logit <- glm(Private ~ ., family= binomial(link='logit'), data= Train)
logit
summary(logit)
#Predicting model

pred_model <- predict(logit, newdata = Test, type = "response")
head(pred_model)
summary(pred_model)

#Prediction Models For test and train data
Predicted_Train<- factor(round((predict(logit, newdata = Train, type = "response"))))
Predicted_Train

predicted_Test<-factor(ifelse(pred_model>.5,1,0))
predicted_Test
# EXpected models for test and train data
expected_Test <- factor(ifelse((Test$Private == "Yes"),1,0))
expected_Test

expected_Train <- factor(ifelse((Train$Private == "Yes"),1,0))
expected_Train

#confusionMatrix for Traindata
confusionMatrix(Predicted_Train,expected_Train)
#confusionMatrix for Testdata
confusionMatrix(predicted_Test, expected_Test)
#ROC
ROCRpred <- prediction(pred_model,Test$Private )
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
#
auc_ROCR <- performance(ROCRpred, measure = "auc")
(auc_ROCR <- auc_ROCR@y.values[[1]])
