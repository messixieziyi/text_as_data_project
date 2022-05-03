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
# library(sophistication)
library(ggplot2)
library(lubridate)

#set working directory
setwd("/Users/xieziyi/Documents/GitHub/text_as_data_project/2nd idea")
getwd()

#read data
data <- read.csv(file = 'us_equities_news_dataset.csv')

#convert to date data type
data$release_date <- ymd(data$release_date)
head(data)
#create dfm
sotu_corpus <- corpus(data$content)

corpus_tokens <- sotu_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_select(min_nchar=2L) %>%
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords('english'))

dfm_object <- dfm_tfidf(dfm(corpus_tokens))

head(dfm_object)
topfeatures(dfm_object)

# plot top features
features_dfm_inaug <- textstat_frequency(dfm_object, n = 25)

features_dfm_inaug$feature <- with(features_dfm_inaug, reorder(feature, -frequency))

ggplot(features_dfm_inaug, aes(x = feature, y = frequency)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# textplot_wordcloud(dfm_object)

#ts
terms_to_observe <- names(topfeatures(dfm_object, 50)) #c("earnings", "price",'growth','higher')
DTM_reduced <- as.matrix(dfm_object[, terms_to_observe])

sum_plus <- function(x) {
  return (sum(x) + 0.000001)
  }

counts_per_day <- aggregate(DTM_reduced, by = list(year = year(data$release_date), month = month(data$release_date)), sum_plus)

temp = counts_per_day[3:length(counts_per_day)]
temp = temp/rowSums(temp)

counts_per_day = cbind(year = counts_per_day$year,month = counts_per_day$month,temp)


counts_per_day$ym <- ym(paste(counts_per_day$year,'',counts_per_day$month))
counts_per_day<-arrange(counts_per_day,counts_per_day$ym)

#plot 

# give x and y values beautiful names
# decades <- counts_per_day$ym
# frequencies <- counts_per_day[, terms_to_observe]
# 
# # plot multiple frequencies
# matplot(decades, frequencies, type = "l")
# 
# # add legend to the plot
# l <- length(terms_to_observe)
# legend('topleft', legend = terms_to_observe, col=1:l, text.col = 1:l, lty = 1:l)  


#correlation


#stock data
library(tidyquant)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

AAPL <- tq_get("AAPL",get  = "stock.prices",from = "2008-10-01", to = "2020-03-15")

AAPL$pct_change <- c(-diff(AAPL$close)/AAPL$close[-1] *  100, NA)

aapl <- aggregate(AAPL$pct_change, by = list(year = year(AAPL$date), month = month(AAPL$date)), mean)

aapl$ym <- ym(paste(aapl$year,'',aapl$month))
aapl<-na.omit(arrange(aapl,aapl$ym))

counts_per_day$year = NULL
counts_per_day$month = NULL

aapl$year = NULL
aapl$month = NULL

merged <- na.omit(merge(x = aapl, y = counts_per_day, by = "ym", all.x = TRUE))


terms_to_observe


N = 1
result <- matrix()

for (word in terms_to_observe) {
  correlation <- cor(merged[['x']],merged[[word]],method = 'spearman')
  result[N] <- correlation
  N<- N+1
}

corr_result <- data.frame(cbind(terms_to_observe,result))


class(corr_result$result) = "Numeric"

corr_result<-arrange(corr_result,desc(corr_result$result))

corr_result
