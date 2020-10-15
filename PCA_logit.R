#---- PCA data �м� �غ��ϱ� ----

# ���� ������ (���� ����) 2957�� �̾���

# 1. ������ �غ��ϱ�

setwd("C:/Users/jaege/Desktop/PCA/Work2")

# Library �ҷ����� ----

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 2657�� => G/A 2149��, train 1505��, test 644��

# 1. ������ �ҷ�����
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

###########################################

# 3. Logistic regression �� ����

# 3-1. �⺻ ��
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              classProbs = TRUE)

LogitModel <- caret::train(PONV ~ .,
                           data = train,
                           method = "LogitBoost",
                           trControl = train.control)
LogitModel
varImp(LogitModel)


# 3. �� ��

# 3-1) �Ʒ� ���� ���� Class ����

train_pred <- predict(LogitModel, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2) �׽�Ʈ ���� ���� Class ����

test_pred <- predict(LogitModel, test)
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes")
confusionMatrix(data = test_pred, reference = test$PONV, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$PONV)


# 3-3) ROC

require(Epi)
require(pROC)

predictedProbs <- predict(LogitModel, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = PONV ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(PONV ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(epiR)
table1 <- as.table(matrix(c(288,255,39,62), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/PCA/ROC/LR_ROC.csv")