library(dplyr)
library(randomForest)
library(mlbench)
library(caret)
library(hrbrthemes)
library(ggplot2)

# setwd("/Users/xieziyi/Documents/GitHub/text_as_data_project")

#### data preperation

shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

merged$target_movement <- tail(c(merged$target_movement,NA),-1)
merged <- na.omit(merged)

samp <- merged[1:nrow(merged),]
rownames(samp) <- NULL
train_samp <- subset(samp, select=-c(ym,x,target_movement,pct_change))

#prepare data
set.seed(1) 
# ids_train <- createDataPartition(1:nrow(samp), p = 0.8, list = FALSE, times = 1)
ids_train<-matrix(1:(0.8*nrow(train_samp)))
train_x_og <- train_samp[ids_train, ] %>% as.data.frame() # train set data
train_y<- merged$target_movement[ids_train] %>% as.factor()  # train set labels

test_x_og <- train_samp[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- merged$target_movement[-ids_train] %>% as.factor() # test set labels

# 0.5302594
baseline_acc <- max(prop.table(table(test_y)))
baseline_acc

for (proportion in c(0.25,0.5,1)){
  
  train_index_max <- ncol(train_x_og)*proportion
  

  train_x <- train_x_og[,1:train_index_max]
  test_x <- test_x_og[,1:train_index_max]
  
  # knn_model
  trctrl <- trainControl(method = "cv",
                         number = 5)
  
  knn_mod <- train(x = train_x,
                          y = train_y,
                          method = "knn",
                          trControl = trctrl,
                          , tuneLength=5)
  
  knn_pred <- predict(knn_mod, newdata = test_x)
  
  knn_cmat <- confusionMatrix(knn_pred, test_y)
  
  knn_cmat$overall[c('Accuracy','AccuracyLower','AccuracyUpper')]
  
  # svm_linear
  svm_mod_linear <- train(x = train_x,
                          y = train_y,
                          method = "svmLinear",
                          trControl = trctrl, tuneLength=5)
  
  svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
  svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y,mode = 'everything')
  
  #naive_bayes model
  ada_model <- train(x = train_x,
                          y = train_y,
                          method = "naive_bayes",
                          trControl = trctrl, tuneLength=3)
  
  
  ada_pred <- predict(ada_model, newdata = test_x)
  ada_cmat <- confusionMatrix(ada_pred, test_y)
  
  #rf
  metric <- "Accuracy"
  mtry <- sqrt(ncol(train_x))
  ntree <- 5 
  rf<- train(x = train_x, y = train_y, method = "rf", 
                               metric = metric, tuneLength = 5, trControl = trctrl)
  rf_pred <- predict(rf, newdata = test_x)
  rf_cmat <- confusionMatrix(rf_pred, test_y)
  
  #summarize
  
  acc_table <- as.data.frame(cbind(model = c('knn','svm','naive_bayes','rf'),
                  rbind(knn_cmat$overall[c('Accuracy','AccuracyLower','AccuracyUpper')],
                  svm_linear_cmat$overall[c('Accuracy','AccuracyLower','AccuracyUpper')],
                  ada_cmat$overall[c('Accuracy','AccuracyLower','AccuracyUpper')],
                  rf_cmat$overall[c('Accuracy','AccuracyLower','AccuracyUpper')]
        )))
  
  acc_table$Accuracy <- as.numeric(acc_table$Accuracy)
  acc_table$AccuracyUpper <- as.numeric(acc_table$AccuracyUpper)
  acc_table$AccuracyLower <- as.numeric(acc_table$AccuracyLower)
  
  print(acc_table)
  
  # ggplot(acc_table, aes(model, Accuracy)) +
  #   geom_point() + geom_errorbar(aes(ymin = AccuracyLower, ymax = AccuracyUpper),width = 0.2)+
  #   geom_hline(yintercept=baseline_acc, linetype="dashed", color = "red") 

}

