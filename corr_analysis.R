#import libraries
rm(list = ls())

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

library(tidyquant)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

#set working directory
setwd("/Users/xieziyi/Documents/GitHub/text_as_data_project")
getwd()

#read data
data <- read.csv(file = 'us_equities_news_dataset.csv')


#convert to date data type
data$release_date <- ymd(data$release_date)

#get aapl
data<-data[data$ticker == 'AAPL',]

#order by date
data <- data[order(data$release_date),]

head(data)

#create dfm
sotu_corpus <- corpus(data$content,docvars = data)

corpus_tokens <- sotu_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_select(min_nchar=2L) %>%
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords('english')) %>% 
  tokens_wordstem

dfm_object <- dfm_tfidf(dfm(corpus_tokens))
# dfm_object <- dfm(corpus_tokens)

# dfm_object <- dfm_trim(dfm_object, max_docfreq = 0.5, docfreq_type = "prop")
# dfm_object <- dfm(tokens_ngrams(corpus_tokens,n=2))

#ts
terms_to_observe <- names(topfeatures(dfm_object, 100)) 
DTM_reduced <- as.matrix(dfm_object[, terms_to_observe])

sum_plus <- function(x) {
  return (sum(x) + 0.000000000)
}

counts_per_day <- aggregate(DTM_reduced, by = list(year = year(data$release_date), month = month(data$release_date), day = day(data$release_date)), sum_plus)

temp = counts_per_day[3:length(counts_per_day)]
temp = temp/rowSums(temp)

counts_per_day = cbind(year = counts_per_day$year,month = counts_per_day$month,day =counts_per_day$day ,temp)
counts_per_day$ym <- ymd(paste(counts_per_day$year,'',counts_per_day$month,'',counts_per_day$day))
#counts_per_day<-arrange(counts_per_day,counts_per_day$ym)

#correlation
#stock data

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

#no lag
N = 1
pearson <- matrix()
kendall <- matrix()
spearman <- matrix()

for (word in terms_to_observe) {
  pearson[N] <- cor(merged[['x']],merged[[word]],method = 'pearson')
  kendall[N] <- cor(merged[['x']],merged[[word]],method = 'kendall')
  spearman[N] <- cor(merged[['x']],merged[[word]],method = 'spearman')
  
  N<- N+1
}

corr_result_no_lag <- data.frame(cbind(terms_to_observe,pearson,kendall,spearman))

corr_result_no_lag[,2:4]<-lapply(corr_result_no_lag[,2:4], as.numeric)

corr_result_no_lag<-arrange(corr_result_no_lag,desc(corr_result_no_lag$kendall))

corr_result_samp <- corr_result_no_lag[seq.int(0, nrow(corr_result_no_lag), by = 5),]

barplot(corr_result_samp$kendall,names = corr_result_samp$terms_to_observe, las=2, xlab = "Term", ylab = "Kendall Correlation No Lag" )



# lag 1
N = 1
pearson <- matrix()
kendall <- matrix()
spearman <- matrix()

for (word in terms_to_observe) {
  pearson[N] <- cor(merged[['x']],head(c(NA,merged[[word]]),-1),method = 'pearson',use = "pairwise.complete.obs")
  kendall[N] <- cor(merged[['x']],head(c(NA,merged[[word]]),-1),method = 'kendall',use = "pairwise.complete.obs")
  spearman[N] <- cor(merged[['x']],head(c(NA,merged[[word]]),-1),method = 'spearman',use = "pairwise.complete.obs")
  
  N<- N+1
}

corr_result_lag_1 <- data.frame(cbind(terms_to_observe,pearson,kendall,spearman))

corr_result_lag_1[,2:4]<-lapply(corr_result_lag_1[,2:4], as.numeric)

corr_result_lag_1<-arrange(corr_result_lag_1,desc(corr_result_lag_1$kendall))

corr_result_samp_lag_1 <- corr_result_lag_1[seq.int(0, nrow(corr_result_lag_1), by = 5),]

barplot(corr_result_samp_lag_1$kendall,names = corr_result_samp_lag_1$terms_to_observe, las=2, ylab = "Kendall Correlation Lag 1" )

# lag 2
N = 1
pearson <- matrix()
kendall <- matrix()
spearman <- matrix()

for (word in terms_to_observe) {
  pearson[N] <- cor(merged[['x']],head(c(NA,NA,merged[[word]]),-2),method = 'pearson',use = "pairwise.complete.obs")
  kendall[N] <- cor(merged[['x']],head(c(NA,NA,merged[[word]]),-2),method = 'kendall',use = "pairwise.complete.obs")
  spearman[N] <- cor(merged[['x']],head(c(NA,NA,merged[[word]]),-2),method = 'spearman',use = "pairwise.complete.obs")
  
  N<- N+1
}

corr_result_lag_2 <- data.frame(cbind(terms_to_observe,pearson,kendall,spearman))

corr_result_lag_2[,2:4]<-lapply(corr_result_lag_2[,2:4], as.numeric)

corr_result_lag_2<-arrange(corr_result_lag_2,desc(corr_result_lag_2$kendall))

corr_result_samp_lag_2 <- corr_result_lag_2[seq.int(0, nrow(corr_result_lag_2), by = 5),]

barplot(corr_result_samp_lag_2$kendall,names = corr_result_samp_lag_2$terms_to_observe, las=2, xlab = "Term", ylab = "Kendall Correlation Lag 2" )


#lag 3
N = 1
pearson <- matrix()
kendall <- matrix()
spearman <- matrix()

for (word in terms_to_observe) {
  pearson[N] <- cor(merged[['x']],head(c(NA,NA,NA,merged[[word]]),-3),method = 'pearson',use = "pairwise.complete.obs")
  kendall[N] <- cor(merged[['x']],head(c(NA,NA,NA,merged[[word]]),-3),method = 'kendall',use = "pairwise.complete.obs")
  spearman[N] <- cor(merged[['x']],head(c(NA,NA,NA,merged[[word]]),-3),method = 'spearman',use = "pairwise.complete.obs")
  
  N<- N+1
}

corr_result_lag_3 <- data.frame(cbind(terms_to_observe,pearson,kendall,spearman))

corr_result_lag_3[,2:4]<-lapply(corr_result_lag_3[,2:4], as.numeric)

corr_result_lag_3<-arrange(corr_result_lag_3,desc(corr_result_lag_3$kendall))

corr_result_samp_lag_3 <- corr_result_lag_3[seq.int(0, nrow(corr_result_lag_3), by = 5),]

barplot(corr_result_samp_lag_3$kendall,names = corr_result_samp_lag_3$terms_to_observe, las=2, xlab = "Term", ylab = "Kendall Correlation Lag 3" )
