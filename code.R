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

# textplot_wordcloud(dfm_object)

#ts
terms_to_observe <- c("nation", "war",'us')
# terms_to_observe <- c('war')
DTM_reduced <- as.matrix(dfm_object[, terms_to_observe])

counts_per_day <- aggregate(DTM_reduced, by = list(year = year(data$publish_date), month = month(data$publish_date)), sum)

counts_per_day$ym <- ym(paste(counts_per_day$year,'',counts_per_day$month))
counts_per_day<-arrange(counts_per_day,counts_per_day$ym)
#plot 

# give x and y values beautiful names
decades <- counts_per_day$ym
frequencies <- counts_per_day[, terms_to_observe]

# plot multiple frequencies
matplot(decades, frequencies, type = "l")

# add legend to the plot
l <- length(terms_to_observe)
legend('topleft', legend = terms_to_observe, col=1:l, text.col = 1:l, lty = 1:l)  

