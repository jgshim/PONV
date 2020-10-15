#---- PCA data 분석 준비하기 ----

# 원본 데이터 (인턴 수집) 2957명 이었음

# 1. 데이터 준비하기

setwd("C:/Users/jaege/Desktop/PCA/Work2")

# 2. 패키지 설치 및 로드하기

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

# Epi패키지의 ROC함수를 써서 PONV와 Apfel 관한 ROC 그래프를 그리기

a1=ROC(form=PONV~Apfel,data=test2,plot="ROC")

a1

# ROC함수는 세개의 list를 반환하는데 첫번째 res는 데이타프레임으로 각각의 lr.eta값에 대해 민감도, 특이도 등이 들어 있읍니다. 최적의 cutoff point는 이중 민감도+특이도의 합이 제일 큰 것

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

# 몸무게로 남여를 구별하는 ROC curve 그리기

a2=ROC(form=PONV~Apfel,data=test2,plot="ROC")

# AUC의 신뢰구간도 알고 싶은 경우에는 Epi패키지로는 알 수 없다. 이 목적으로는 pROC패키지의 roc함수를 쓰면 됩니다.

b1=roc(PONV~Apfel,test2,ci=T,percent=T)
b2=roc(PONV~Apfel,test2,ci=T,percent=T)

plot(b1)

b1
plot(b2,add=TRUE,col="red")

# 또한 두개의 커브가 다른지 검정도 하고 싶습니다. 이떄에는 역시 pROC패키지의 roc.test를 사용

roc.test(b1,b2,plot=T)

# AUC가 0.5보다 큰지 검정도 하고 싶습니다. 이때에는 wilcox.test를 하면 됨

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

