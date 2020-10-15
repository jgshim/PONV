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
# 2. Decision Tree 모델 적용

# 2-1. 기본 모델

train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              savePredictions = TRUE)

TreeModel1 <- caret::train(PONV ~ ., data = train,
                           method="rpart",
                           metric = "Accuracy",
                           trControl=train.control)

TreeModel1
TreeModel1$finalModel

varImp(TreeModel1)
plot(TreeModel1$finalModel, uniform=TRUE, main="Classification Tree")
text(TreeModel1$finalModel, use.n.=TRUE, all=TRUE, cex=.5)

suppressMessages(library(rattle))
fancyRpartPlot(TreeModel1$finalModel)

library(rpart.plot)
rpart.plot(TreeModel1$finalModel)

# 2-2. 그리드탐색

Grid <- expand.grid(cp=0:1/10) 

set.seed(42)
TreeMode2 <- train(LBP ~ ., data=train,
                   method="rpart",
                   trControl=train.control,
                   metric = "Accuracy",
                   tuneGrid = Grid)

TreeMode2

trellis.par.set(caretTheme())
plot(TreeMode2) 
plot(TreeMode2, metric = "Kappa")
plot(TreeMode2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(TreeMode2)  

# 2-3. 랜덤 서치 기반 모델 튜닝

train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 3,
                              classProbs = TRUE,
                              search = "random")

set.seed(42)
TreeModel3 <- caret::train(PONV ~ ., data = train, 
                    method = "rpart",
                    metric = "Accuracy",
                    tuneLength = 30,
                    trControl = train.control)
TreeModel3

plot(TreeModel3) 
plot(TreeModel3, metric = "Kappa")
plot(TreeModel3, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(TreeModel3)  

# 2-4. 최종모델의 선택

finalControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(42)
FinalModel <- caret::train(PONV ~ ., data = train, 
                           method = "rpart", 
                           trControl = finalControl, 
                           tuneGrid = data.frame(cp=0.0180084),
                           metric = "ROC")
FinalModel


# 3. 모델 평가

# 3-1. 훈련 모델의 예측 Class 측정

train_pred <- predict(FinalModel, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2. 테스트 모델의 예측 Class 측정

test_pred <- predict(FinalModel, test)
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes")
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$PONV)

# 3-3. ROC
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
table1 <- as.table(matrix(c(304,239,51,50), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/PCA/ROC/DT_ROC.csv")
