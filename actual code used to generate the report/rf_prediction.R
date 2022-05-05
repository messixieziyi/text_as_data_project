# rm(list = ls())

library(dplyr)
library(randomForest)
library(mlbench)
library(caret)
library(hrbrthemes)
library(ggplot2)

setwd("/Users/xieziyi/Documents/GitHub/text_as_data_project")


# merged <- load("merged_data.Rdata")

# baseline: 0.5304282 0.4695718 
prop.table(table(merged$target_movement))
 
samp <- merged %>% sample_n(nrow(merged))
rownames(samp) <- NULL
train_samp <- subset(samp, select=-c(ym,x,target_movement))
#prepare data
ids_train <- createDataPartition(1:nrow(samp), p = 0.8, list = FALSE, times = 1)
train_x <- train_samp[ids_train, ] %>% as.data.frame() # train set data
train_y <- merged$target_movement[ids_train] %>% as.factor()  # train set labels

test_x <- train_samp[-ids_train, ]  %>% as.data.frame() # test set data
test_y <- merged$target_movement[-ids_train] %>% as.factor() # test set labels

#use rf
mtry <- sqrt(ncol(train_x))
ncol(train_x)
mtry
# mtry <- 10
ntree <- 5  # num of trees to grow

system.time(rf.base <- randomForest(x = train_x, y = train_y, ntree = ntree, mtry = mtry, importance = TRUE))
token_importance <- round(importance(rf.base, 2), 2)
head(rownames(token_importance)[order(-token_importance)])

# print results
print(rf.base)

varImpPlot(rf.base, n.var = 10, main = "Variable Importance")

predict_test <- predict(rf.base, newdata = test_x)
confusionMatrix(data = predict_test, reference = test_y)

trainControl <- trainControl(method = "cv", number = 5)

metric <- "Accuracy"
tunegrid <- expand.grid(.mtry = c(0.5*mtry, mtry, 1.5*mtry,2*mtry,2.5*mtry,3*mtry,3.5*mtry,4*mtry,4.5*mtry,5*mtry))  
# at the moment caret only allows tuning of mtry 
# (partly b/c ntree is just a matter of computational constratints)
system.time(rf.grid <- train(x = train_x, y = train_y, method = "rf", 
                             metric = metric, tuneGrid = tunegrid, trControl = trainControl, 
                             ntree = ntree)
)
# print grid search results
print(rf.grid)

# plot grid search results
plot(rf.grid)








