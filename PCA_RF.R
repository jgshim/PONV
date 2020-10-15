#---- PCA data 분석 준비하기 ----

# 원본 데이터 (인턴 수집) 2957명 이었음

# 1. 데이터 준비하기

setwd("C:/Users/jaege/Desktop/PCA/Work2")

# Library 불러오기 ----

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 2657명, train 1861명, test 796명

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


# 2. Random Forest 모델 적용

# 2-1. 기본 모델
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              savePredictions = TRUE,
                              classProbs =  TRUE)

RFModel1 <- caret::train(PONV ~ ., data=train,
                         method="rf",
                         trControl=train.control,
                         tuneLength = 10)

RFModel1
varImp(RFModel1)

# estimate variable importance
importance <- varImp(RFModel1)
# summarize importance
print(importance)
# plot importance
plot(importance)


# 2-2. 그리드탐색

Grid <- expand.grid(mtry = c(1:14)) 

set.seed(42)
train.control <- trainControl(method = "repeatedcv", 
                              number = 5, 
                              repeats = 10,
                              savePredictions = TRUE)

RFModel2 <- train(LBP ~ ., data=train,
                  method="rf",
                  trControl=train.control,
                  metric = "Accuracy",
                  tuneGrid = Grid)
RFModel2

trellis.par.set(caretTheme())
plot(RFModel2) 
plot(RFModel2, metric = "Kappa")
plot(RFModel2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(RFModel2) 

# 2-3. 랜덤 서치 기반 모델 튜닝

train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 10,
                              classProbs = TRUE,
                              search = "random")
set.seed(42)
RFModel3 <- train(LBP ~ ., data = train, 
                  method = "rf",
                  metric = "Accuracy",
                  tuneLength = 30,
                  trControl = train.control)
RFModel3

plot(RFModel3) 
plot(RFModel3, metric = "Kappa")
plot(RFModel3, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(RFModel3) 

# 2-4. 최종모델의 선택

finalControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(42)
FinalModel <- caret::train(PONV ~ ., data = train, 
                           method = "rf", 
                           trControl = finalControl, 
                           tuneGrid = data.frame(mtry=3),
                           metric = "ROC")

# 3. 모델 평가

# 3-1) 훈련 모델의 예측 Class 측정

train_pred <- predict(FinalModel, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2) 테스트 모델의 예측 Class 측정

test_pred <- predict(FinalModel, test)
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes")
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$PONV)


# 3-3) ROC

require(Epi)
require(pROC)

predictedProbs <- predict(FinalModel, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = PONV ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(PONV ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1
library(ROCR)
library(epiR)
table1 <- as.table(matrix(c(372,171,53,48), nrow = 2, byrow = TRUE))
epi.tests(table1)


write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/PCA/ROC/RF_ROC.csv")
