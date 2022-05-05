setwd("/Users/xieziyi/Documents/GitHub/text_as_data_project")
getwd()

#read data
data <- read.csv(file = 'us_equities_news_dataset.csv')


#convert to date data type
data$release_date <- ymd(data$release_date)

#get aapl
data<-data[data$ticker == 'AAPL',]

counts_provider <- table(data$provider)

sort(counts_provider,decreasing = TRUE)[1:10]


table(data$category)


sentence_sample = kwic(tokens(data$content),pattern= c('increase'),window = 3)

sample(sentence_sample,size = 10)
