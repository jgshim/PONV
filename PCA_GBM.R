#---- PCA data �м� �غ��ϱ� ----

# ���� ������ (���� ����) 2957�� �̾���

# 1. ������ �غ��ϱ�

setwd("C:/Users/jaege/Desktop/PCA/Work2")

# Library �ҷ����� ----

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 2657��, train 1861��, test 796��

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

# 2. Gradient Boosting �� ����

# Fit the model on the training set
set.seed(42)

train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              savePredictions = TRUE)

# This is the grid space to search for the best hyperparameters.
# The hyperparameters to optimize are found in the website.
xgbGrid <- expand.grid(nrounds = c(100,200),
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)


model <- caret::train(PONV ~ ., data = train,
                      method = "xgbTree",
                      trControl = train.control)

model
varImp(model)

# Best tuning parameter mtry
model$bestTune


# 3. �� ��

# 3-1. �Ʒ� ���� ���� Class ����

train_pred <- predict(model, train)
confusionMatrix(data = train_pred, reference = train$PONV)
confusionMatrix(data = train_pred, reference = train$PONV, mode = "prec_recall")
postResample(pred = train_pred, obs = train$PONV)

# 3-2. �׽�Ʈ ���� ���� Class ����

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

library(epiR)
table1 <- as.table(matrix(c(341,202,54,47), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/PCA/ROC/GBM_ROC.csv")