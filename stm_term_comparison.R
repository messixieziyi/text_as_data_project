# svm stm model with 10 topics 
blogs_dfm_stm <- convert(dfm(corpus_tokens), to = "stm", docvars = docvars(sotu_corpus))

stm_object <- stm(documents = blogs_dfm_stm$documents, 
                  vocab = blogs_dfm_stm$vocab, 
                  data = blogs_dfm_stm$meta,
                  K = 5, 
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

merged_topic <- na.omit(merge(x = AAPL, y = topic_prop_mean, by = "date", all.x = TRUE))

merged_topic$target <- as.numeric(merged_topic$pct_change>0)

#prepare data for model
merged_topic<- head(merged_topic,-1)

merged_topic$target <- tail(c(merged_topic$target,NA),-1)
merged_topic <- na.omit(merged_topic)

y_s <- subset(merged_topic,select = target)
x_s <- subset(merged_topic, select = -c(date,pct_change,docnum,target))

set.seed(1) 
# ids_train <- createDataPartition(1:nrow(x_s), p = 0.8, list = FALSE, times = 1)
ids_train <- matrix(1:(0.8*nrow(x_s)))
train_x_topic <- x_s[ids_train, ] %>% as.data.frame() # train set data
train_y_topic <- y_s[ids_train] %>% as.factor()  # train set labels

test_x_topic <- x_s[-ids_train, ]  %>% as.data.frame() # test set data
test_y_topic <- y_s[-ids_train] %>% as.factor() # test set labels

baseline_acc <- max(prop.table(table(test_y_topic)))

baseline_acc
#0.5545977

# the models 

#svm model
trctrl <- trainControl(method = "cv",
                       number = 5)
svm_mod_linear_topic <- train(x = train_x_topic,
                        y = train_y_topic,
                        method = "svmLinear",
                        trControl = trctrl, tuneLength=5)

svm_linear_pred_topic <- predict(svm_mod_linear_topic, newdata = test_x_topic)
svm_linear_cmat_topic <- confusionMatrix(svm_linear_pred_topic, test_y_topic,mode = 'everything')
svm_linear_cmat_topic
 
# svm term freq model with top 50 terms

#merged
AAPL <- tq_get("AAPL",get  = "stock.prices",from = "2012-01-01", to = "2020-02-01")

AAPL$pct_change <- c(-diff(AAPL$close)/AAPL$close[-1] *  100, NA)

aapl <- aggregate(AAPL$close, by = list(year = year(AAPL$date), month = month(AAPL$date),day = day(AAPL$date)), mean)

aapl$ym <- ymd(paste(aapl$year,'',aapl$month,'',aapl$day))
aapl<-na.omit(arrange(aapl,aapl$ym))

counts_per_day$year = NULL
counts_per_day$month = NULL
counts_per_day$day = NULL

aapl$year = NULL
aapl$month = NULL
aapl$day = NULL

merged <- na.omit(merge(x = aapl, y = counts_per_day, by = "ym", all.x = TRUE))
merged$pct_change <- c(-diff(merged$x)/merged$x[-1] *  100, NA)
merged <- na.omit(merged)
merged$target_movement = as.numeric(merged$pct_change >0)

merged$target_movement <- tail(c(merged$target_movement,NA),-1)
merged <- na.omit(merged)

samp <- merged[1:nrow(merged),] #%>% sample_n(nrow(merged)-1)
train_samp <- subset(samp, select=-c(ym,x,pct_change,target_movement))
rownames(samp) <- NULL
#prepare data
train_index_max <- ncol(train_x_og)*0.5

train_x <- (train_samp[ids_train, ] %>% as.data.frame())[,1:train_index_max]
train_y <- merged$target_movement[ids_train] %>% as.factor()  

test_x <- (train_samp[-ids_train, ]  %>% as.data.frame())[,1:train_index_max]
test_y <- merged$target_movement[-ids_train] %>% as.factor() 
prop.table(table(test_y))

svm_mod_linear <- train(x = train_x,
                        y = train_y,
                        method = "naive_bayes",
                        trControl = trctrl, tuneLength=3)

svm_linear_pred <- predict(svm_mod_linear, newdata = test_x)
svm_linear_cmat <- confusionMatrix(svm_linear_pred, test_y,mode = 'everything')
svm_linear_cmat



#importance



varImp(svm_mod_linear, scale = TRUE)

varImp(svm_mod_linear_topic, scale = TRUE)

labelTopics(stm_object,n=8)$prob

