#---- PCA data �м� �غ��ϱ� ----

# ���� ������ (���� ����) 2957�� �̾���

# 1. ������ �غ��ϱ�

setwd("C:/Users/jaege/Desktop/PCA/Work2")

# 2. ��Ű�� ��ġ �� �ε��ϱ�

library(tidyverse)
library(caret)

data <- read.csv("PCA_data_p2.csv", header=TRUE)
data <- data[ , c(2:37)]
head(data)

data <- data %>% filter(type_an == 1)

set.seed(42)
training.samples <- createDataPartition(data$PONV, p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

table(train$PONV)
table(test$PONV)

# prepare training scheme

test2 <- select(test,sex,smoking,motion_sickness,ponv,PONV)

test2$motion_ponv <- test2$motion_sickness + test2$ponv
test2$motion_ponv <- ifelse(test2$motion_ponv >=1, 1, 0)
test2$sex <- ifelse(test2$sex == 1, 0, 1)
test2$smoking <- ifelse(test2$smoking == 1, 0, 1)

test2$Apfel <- test2$sex + test2$motion_ponv + test2$smoking + 1
summary(test2$Apfel)
Apfel_ROC <- select(test2, Apfel)

# ROC curve analysis

require(Epi)
require(pROC)
require(ztable)
require(moonBook)

# Epi��Ű���� ROC�Լ��� �Ἥ PONV�� Apfel ���� ROC �׷����� �׸���

a1=ROC(form=PONV~Apfel,data=test2,plot="ROC")

a1

# ROC�Լ��� ������ list�� ��ȯ�ϴµ� ù��° res�� ����Ÿ���������� ������ lr.eta���� ���� �ΰ���, Ư�̵� ���� ��� �����ϴ�. ������ cutoff point�� ���� �ΰ���+Ư�̵��� ���� ���� ū ��

optimal_lr.eta <-  function(x) {
  no = which.max(x$res$sens+x$res$spec)[1]
  result = x$res$lr.eta[no]
  result
}
optimal_lr.eta(a1)

optimal_cutpoint <-  function(x) {
  y = optimal_lr.eta(x)
  b0 = unname(x$lr$coeff[1])
  b1 = unname(x$lr$coeff[2])
  result = (-log(1/y-1)-b0)/b1
  result
} 

optimal_cutpoint(a1) 

# �����Է� ������ �����ϴ� ROC curve �׸���

a2=ROC(form=PONV~Apfel,data=test2,plot="ROC")

# AUC�� �ŷڱ����� �˰� ���� ��쿡�� Epi��Ű���δ� �� �� ����. �� �������δ� pROC��Ű���� roc�Լ��� ���� �˴ϴ�.

b1=roc(PONV~Apfel,test2,ci=T,percent=T)
b2=roc(PONV~Apfel,test2,ci=T,percent=T)

plot(b1)

b1
plot(b2,add=TRUE,col="red")

# ���� �ΰ��� Ŀ�갡 �ٸ��� ������ �ϰ� �ͽ��ϴ�. �̋����� ���� pROC��Ű���� roc.test�� ���

roc.test(b1,b2,plot=T)

# AUC�� 0.5���� ū�� ������ �ϰ� �ͽ��ϴ�. �̶����� wilcox.test�� �ϸ� ��

wilcox.test(height~male,data=radial)


pred_test <- ifelse(test2$Apfel > 2, 1, 0)
pred_test <- as.data.frame(pred_test)

write.csv(pred_test, file="C:/Users/jaege/Desktop/PCA/ROC/Apfel_ROC.csv", row.names = TRUE)

test$PONV <- as.numeric(test$PONV)
test$PONV <- ifelse(test$PONV == 1, 0, 1)

library(gmodels)
CrossTable(x=pred_test$pred_test, y=test$PONV, chisq = T)
confusionMatrix(table(as.vector(pred_test$pred_test), as.vector(test$PONV)), positive = "1")
confusionMatrix(table(as.vector(pred_test$pred_test), as.vector(test$PONV)), positive = "1", mode = "prec_recall")

# ROC

library(epiR)
table1 <- as.table(matrix(c(260,283,24,77), nrow = 2, byrow = TRUE))
epi.tests(table1)
