#import libraries
library(gutenbergr)
library(stylest)
library(dplyr)
library(stringi)
library(stringr)
library(pbapply)
library(corpus)
library(quanteda)
library(quanteda.textstats)
library(quanteda.corpora)
library("quanteda.textplots")
library(ggplot2)
library(lubridate)
library(prophet)
library(tsibble)
library(fable)



#set working directory
setwd("/Users/xieziyi/Documents/GitHub/text_as_data_project")
getwd()

#read data
data <- read.csv(file = 'abcnews-date-text.csv')

#convert to date data type
data$publish_date <- ymd(data$publish_date)

#create dfm
sotu_corpus <- corpus(data$headline_text)

corpus_tokens <- sotu_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords('english'))

dfm_object <- dfm(corpus_tokens) 
(topfeatures(dfm_object))
# textplot_wordcloud(dfm_object)

#ts
terms_to_observe <- names(topfeatures(dfm_object))
# terms_to_observe <- c('war')
DTM_reduced <- as.matrix(dfm_object[, terms_to_observe])

counts_per_day <- aggregate(DTM_reduced, by = list(date = data$publish_date), sum)

#use prophet for prediction
train <- counts_per_day[,c('police','date')]
names(train)<- c('y', 'ds') 
train <- as_tsibble(train)
m <- prophet::prophet(train)
future <- make_future_dataframe(m, periods=1000, freq ='day')
forecast<-predict(m, future)
plot(m, forecast)


#monthly agg
forecast_tmp <-forecast[,c('ds','yhat')]%>%mutate(ds = as.Date(ds)) %>%as_tsibble(index = ds)
forecast_tmp <-forecast_tmp%>%index_by(Year_Month = ~ yearmonth(.))%>%summarise(sum = sum(yhat))

train_tmp <- train[,c('ds','y')]
train_tmp <- train_tmp%>%index_by(Year_Month = ~ yearmonth(.))%>%summarise(sum = sum(y))

combined_tmp <- forecast_tmp %>% left_join(train_tmp,by = 'Year_Month') 


combined_tmp%>%autoplot(sum.x) + combined_tmp%>%autoplot(sum.y) 

plot(as.list(combined_tmp))


p1<-train_tmp%>%autoplot(vars('sum'))
p2<-forecast_tmp%>%autoplot(vars('sum'))

ggplot(train_tmp,aes(x = Year_Month,y = sum)) +geom_line() +ggplot(forecast_tmp,aes(x = Year_Month,y = sum))


#evaluation
train.cv <- cross_validation(m, initial = 366*2, period = 366, horizon = 366, units = 'days')

train.p <- performance_metrics(train.cv,rolling_window = 0)
head(train.p)
plot_cross_validation_metric(train.cv, metric = 'mae')


