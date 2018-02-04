getwd()
setwd('/path/Raw Data')

library(rpart)
library(MASS)
library(tree)
library(party)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(ROCR)

train<-read.csv('train.csv')
test<-read.csv('test.csv')

str(train)
str(test)
#changing the variables to factors

train$Occupation <- factor(train$Occupation)
train$Marital_Status <- factor(train$Marital_Status)
train$Product_Category_1 <- factor(train$Product_Category_1)
train$Product_Category_2<- factor(train$Product_Category_2)
train$Product_Category_3<-factor(train$Product_Category_3)

test$Occupation <- factor(test$Occupation)
test$Marital_Status <- factor(test$Marital_Status)
test$Product_Category_1 <- factor(test$Product_Category_1)
test$Product_Category_2<- factor(test$Product_Category_2)
test$Product_Category_3<-factor(test$Product_Category_3)

#creating a training and validation subsets

set.seed(1)
index <- sample(1:nrow(train), nrow(train)*0.8)
training<- train[index,]
validation<- train[-index,]

#feature engineering

training$Product <- 3 - is.na(training$Product_Category_2)- is.na(training$Product_Category_3)
validation$Product <- 3 - is.na(validation$Product_Category_2)- is.na(validation$Product_Category_3) 
test$Product <- 3 - is.na(test$Product_Category_2)- is.na(test$Product_Category_3)
val_actual<-validation$Purchase
head(val_actual)
length(val_actual)

###running the model using LM

model_lm <- lm(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                 Marital_Status+Product_Category_1+Product , data = training)
summary(model_lm)
#r-square = 64.03, RMSE = 3014
#predicting the validation dataset
pred_lm <- predict(model_lm, validation)
length(pred_lm)
#rmse value checking for validation dataset
rmse <- sqrt(mean((pred_lm - val_actual)^2))
rmse #3004.57
#predicting on the test dataset 
pred_test_lm <- predict(model_lm, test)
test_sub_lm<- data.frame(test$User_ID,test$Product_ID,pred_test_lm)
head(test_sub_lm)
str(training)

###running the model using Decsion Trees

model_dt <- rpart(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                    Marital_Status+Product_Category_1+Product, data = training)
summary(model_dt)
printcp(model_dt)
plotcp(model_dt)

#plotting the tree
plot(model_dt)
text(model_dt, pretty = 0)

#pruning the tree
model_dt_prune<- prune(model_dt, cp=0.01)

# make predictions
pred_dt <- predict(model_dt_prune, validation)
length(pred_dt)
rmse_dt <- sqrt(mean((pred_dt - val_actual)^2))
rmse_dt

#predicting on the test dataset 
pred_test_dt <- predict(model_dt, test)
test_sub_dt<- data.frame(test$User_ID,test$Product_ID,pred_test_dt)
head(test_sub_dt)

###running the model using random forest

set.seed(1)
model_rf <- randomForest(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+
                           Marital_Status+Product_Category_1+Product, data = training, ntree = 50)
summary(model_rf)

#predicting on the test dataset 
pred_test_rf <- predict(model_rf, test)
test_sub_rf <- data.frame(test$User_ID,test$Product_ID,pred_test_rf)
head(test_sub_rf)