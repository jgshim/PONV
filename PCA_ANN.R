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

table(data$type_an)

data <- data %>% filter(type_an == 1)

# G/A 2149명만 추출

# prepare training scheme

data <- select(data,motion_sickness,main_fentanyl,age,bmi,asa,ponv,premedi,type_op.7,duration_an,sex,lapa,htn,pre_op,PONV)

set.seed(42)
training.samples <- createDataPartition(data$PONV, p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

table(train$PONV)
table(test$PONV)

# Up
up_train <- upSample(x = train[, -ncol(train)],
                     y = train$PONV)
up_train <- rename(up_train, PONV = Class)
table(up_train$PONV)
data_train <- up_train
data_test <- test

# ROSE
library(ROSE)
set.seed(42)
rose_train <- ROSE(PONV ~ ., data  = train)$data
table(rose_train$PONV)
data_train <- rose_train
table(data_train$PONV)
data_test <- test

# SMOTE
train2 <- SMOTE(PONV~., train, perc.over = 100, perc.under=200)
table(train2$PONV)
data_train <- train2
data_test <- test

###########################################

### 케라스 모델

library(tensorflow)
library(ggplot2)
library(caTools)

str(data_train$PONV)
class(data_train$PONV)
data_train$PONV <- as.numeric(data_train$PONV)
table(data_train$PONV)
data_train$PONV <- ifelse(data_train$PONV==1, 0, 1)
data_test$PONV <- as.numeric(data_test$PONV)
data_test$PONV <- ifelse(data_test$PONV==1, 0, 1)
str(data_train$PONV)
str(data_test$PONV)
table(data_train$PONV)
table(data_test$PONV)

input <- as.matrix(data_train[1:13], ncol = 13)
output <- as.matrix(data_train[14], ncol = 1)
input2 <- as.matrix(data_test[1:13], ncol = 13)
output2 <- as.matrix(data_test[14], ncol = 1)

#3. seed 고정

seed <- 42
set.seed(seed)

#4. train set / test set split

input_train <- input
input_test <- input2
output_train <- output
output_test <- output2
str(output_test)


library(keras)

# 모델 정의하기
# 동일한 모델을 여러 번 인스턴스화해야 하기 때문에 함수를 사용해 모델을 생성한다.
build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 26, kernel_regularizer = regularizer_l2(0.001), activation = "relu") %>% 
    layer_dense(units = 12, kernel_regularizer = regularizer_l2(0.001), activation = "relu") %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


# k겹 검증하기
k <- 10
indices <- sample(1:nrow(input_train))
folds <- cut(1:length(indices), breaks = k, labels = FALSE)

num_epochs <- 200
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- input_train[val_indices,]
  val_targets <- output_train[val_indices]
  
  partial_train_data <- input_train[-val_indices,]
  partial_train_targets <- output_train[-val_indices]
  
  model <- build_model()
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 20, verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results$accuracy)
}

all_scores
mean(all_scores)

num_epochs <- 200
all_acc_histories <- NULL
all_loss_histories <- NULL

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- input_train[val_indices,]
  val_targets <- output_train[val_indices]
  
  partial_train_data <- input_train[-val_indices,]
  partial_train_targets <- output_train[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 20, verbose = 0
  )
  acc_history <- history$metrics$val_acc
  all_acc_histories <- rbind(all_acc_histories, acc_history)
  loss_history <- history$metrics$val_loss
  all_loss_histories <- rbind(all_loss_histories, loss_history)
}

# 연속 평균 k겹 검증 점수의 이력 구축하기
average_acc_history <- data.frame(
  epoch = seq(1:ncol(all_acc_histories)),
  validation_acc = apply(all_acc_histories, 2, mean)
)
average_loss_history <- data.frame(
  epoch = seq(1:ncol(all_loss_histories)),
  validation_loss = apply(all_loss_histories, 2, mean)
)

# 검증 점수 그리기
ggplot(average_acc_history, aes(x = epoch, y = validation_acc)) + geom_line()
ggplot(average_loss_history, aes(x = epoch, y = validation_loss)) + geom_line()

# geom_smooth()로 검증 점수를 그리기
ggplot(average_acc_history, aes(x = epoch, y = validation_acc)) + geom_smooth()
ggplot(average_loss_history, aes(x = epoch, y = validation_loss)) + geom_smooth()

# 최종 모델 훈련하기
model <- build_model()
model %>% fit(input_train, output_train,
              epochs = 15, batch_size = 20, verbose = 0)
results <- model %>% evaluate(input_test, output_test)
model %>% predict(input_test) -> pred_test

class(pred_test)
class(output_test)
ANN_ROC <- data.frame(x = pred_test, y = output_test)
write.csv(ANN_ROC, file = "C:/Users/jaege/Desktop/PCA/ROC/ANN_ROC.csv")

require(Epi)
require(pROC)

output_test_df <- as.data.frame(output_test)
a1 = ROC(form = PONV ~ pred_test, data = output_test_df, plot="ROC")
b1 = roc(PONV ~ pred_test, output_test_df, ci=T, percent=T)

plot(b1)

b1
a1

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

pred_test <- ifelse(pred_test > 0.4722573, 1, 0)
pred_test <- as.data.frame(pred_test)
  
library(gmodels)
CrossTable(x=pred_test$V1, y=output_test_df$PONV, chisq = T)
confusionMatrix(table(as.vector(pred_test$V1), as.vector(output_test_df$PONV)), positive = "1")
confusionMatrix(table(as.vector(pred_test$V1), as.vector(output_test_df$PONV)), positive = "1", mode = "prec_recall")

# ROC

library(epiR)
table1 <- as.table(matrix(c(306,237,25,76), nrow = 2, byrow = TRUE))
epi.tests(table1)


