#stm model

library("ggplot2")        
library(dplyr)
library(randomForest)
library(mlbench)
library(caret)
library(hrbrthemes)
library(ggplot2)
library("lubridate")
library(stm)
library(quanteda)
library(topicmodels)
library(tidytext)

dfm_object <- dfm(corpus_tokens)

# docvars(dfm_object) <-NULL

blogs_dfm_stm <- convert(dfm(corpus_tokens), to = "stm", docvars = docvars(sotu_corpus))

# blogs_dfm_stm <- convert(dfm_object, to = "stm")#, docvars = docvars(sotu_corpus))

acc_list<- list()
topic_size = 5
for (topic_size in c(5,10,25)){

  stm_object <- stm(documents = blogs_dfm_stm$documents, 
                    vocab = blogs_dfm_stm$vocab, 
                    data = blogs_dfm_stm$meta,
                    K = topic_size, 
                    seed = 12345,emtol = 0.0001)#,max.em.its = 10)
  
  # plot(stm_object)
  
  #topic proportions
  topic_prop <- make.dt(stm_object, meta = NULL)
  # topic_prop$date <- dfm_object$release_date
  
  topic_prop_mean <- aggregate(topic_prop,
            by = list(dfm_object$release_date),
            FUN = mean)
  
  colnames(topic_prop_mean)[1] <- 'date'
  
  AAPL <- tq_get("AAPL",get  = "stock.prices",from = "2012-01-01", to = "2020-02-01")
  
  AAPL$pct_change <- c(-diff(AAPL$close)/AAPL$close[-1] *  100, NA)
  
  AAPL <- subset(AAPL,select = c('date','pct_change'))
  
  merged <- na.omit(merge(x = AAPL, y = topic_prop_mean, by = "date", all.x = TRUE))
  
  
  merged$target <- as.numeric(merged$pct_change>0)
  
  merged$target <- tail(c(merged$target,NA),-1)
  merged <- na.omit(merged)
  
  

  #prepare data for model
  
  y_s <- subset(merged,select = target)
  x_s <- subset(merged, select = -c(date,pct_change,docnum,target))
  
  ids_train <- ids_train<-matrix(1:(0.8*nrow(y_s)))
  train_x <- x_s[ids_train, ] %>% as.data.frame() # train set data
  train_y <- y_s[ids_train] %>% as.factor()  # train set labels
  
  test_x <- x_s[-ids_train, ]  %>% as.data.frame() # test set data
  test_y <- y_s[-ids_train] %>% as.factor() # test set labels
  
  baseline_acc <- max(prop.table(table(test_y)))
  #0.5488506
  
  # the models 
  
  #svm model
  trctrl <- trainControl(method = "cv",
                         number = 5)
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
  
  print(topic_size)
  print(acc_table)
  
  print(svm_linear_cmat$overall)

  }
