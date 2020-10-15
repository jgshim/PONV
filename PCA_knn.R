#---- PCA data 분석 준비하기 ----

# 원본 데이터 (인턴 수집) 2957명 이었음

# 1. 데이터 준비하기

setwd("C:/Users/jaege/Desktop/PCA/Work2")

# Library 불러오기 ----

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 2657명 => G/A 2149명, train 1505명, test 644명

# 1. 데이터 불러오기
data <- read.csv("PCA_data_p2.csv", header=TRUE)
data <- data[ , c(2:37)]
head(data)

data <- data %>% filter(type_an == 1)

# prepare training scheme

data <- select(data,motion_sickness,main_fentanyl,age,bmi,asa,ponv,premedi,type_op.7,duration_an,sex,lapa,htn,pre_op,PONV)

set.seed(42)
training.samples <- createDataPartition(data$PONV, p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

table(train$PONV)
table(test$PONV)

# SMOTE
train2 <- SMOTE(PONV~., train, perc.over = 100, perc.under=200)
table(train2$PONV)
train <- train2

# 2. k-nearest neighbors (KNN) 모델 적용

# The k-nearest neighbors (KNN) algorithm is a simple machine learning method used for both classification and regression.
# The kNN algorithm predicts the outcome of a new observation by comparing it to k similar cases in the training data set, where k is defined by the analyst.

# Fit the model on the training set
set.seed(42)

train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 3,
                              savePredictions = TRUE)

model <- caret::train(PONV ~ ., data = train,
                      method = "knn",
                      trControl = train.control,
                      tuneLength = 20)

model

# Plot model accuracy vs different values of k
plot(model)

# Print the best tuning parameter k that maximizes model accuracy
model$bestTune

# 3. 모델 평가

# 3-1. 훈련 모델의 예측 Class 측정

train_pred <- predict(model, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2. 테스트 모델의 예측 Class 측정

test_pred <- predict(model, test)
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes")
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$PONV)

# 3-3. ROC

require(Epi)
require(pROC)

predictedProbs <- predict(model, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = PONV ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(PONV ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(ROCR)
library(epiR)
table1 <- as.table(matrix(c(375,168,58,43), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/PCA/ROC/NB_ROC.csv")
