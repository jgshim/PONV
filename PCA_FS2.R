# Library 불러오기 ----

setwd("C:/Users/jaege/Desktop/PCA/Work2")

library(caret)
library(tidyverse)
library(MASS)

########## Binary classification ###########
# 원본 데이터 2957명 이었음
# total 3150명, train 4284명, test 1835명

# 1. 데이터 불러오기
data <- read.csv("PCA_data_p2.csv", header=TRUE)
data <- data[ , c(2:36)]
str(data)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=5)
# run the RFE algorithm
results <- rfe(data[,1:34], data[,35], sizes=c(1:34), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

importance <- varImp(results, scale=FALSE)
print(importance)
plot(results)
ggplot(results)

# prepare training scheme

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- caret::train(PONV~., data=data, method="lvq",trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

write.csv(importance$importance, file="PCA_importance.csv", row.names = TRUE)
