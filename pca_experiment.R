library(quanteda)
library(quanteda.corpora)
library(dplyr)
# makes it easy to work with PCA (great for visualization)
library(factoextra) 

library(text2vec)
library(lsa)

####Data Prep
data <- read.csv(file = 'us_equities_news_dataset.csv')
data$release_date <- ymd(data$release_date)
head(data)
#create dfm
sotu_corpus <- corpus(data$content)

corpus_tokens <- sotu_corpus %>% 
  tokens(remove_punct = TRUE, remove_numbers = TRUE, remove_symbols = TRUE) %>% 
  tokens_select(min_nchar=2L) %>%
  tokens_tolower() %>% 
  tokens_remove(pattern = stopwords('english'))

# dfm_object <- dfm_tfidf(dfm(corpus_tokens))
dfm_object <- dfm(corpus_tokens)
dfm_object <- dfm_trim(dfm_object, max_docfreq = 0.5, docfreq_type = "prop")

terms_to_observe <- names(topfeatures(dfm_object, 100)) #c("earnings", "price",'growth','higher')
DTM_reduced <- as.matrix(dfm_object[, terms_to_observe])

#### start PCA

SOTU_mat <- DTM_reduced

# run pca
SOTU_pca <- prcomp(SOTU_mat, center = TRUE, scale = TRUE)

SOTU_pca

summary(SOTU_pca)
dim(SOTU_pca$x)
View(SOTU_pca$x[1:100,1:3])

fviz_eig(SOTU_pca, addlabels = TRUE)

SOTU_pca$rotation[1:10, 1:5]

pc_loadings <- SOTU_pca$rotation

# what do we expect this correlation to be?
cor(pc_loadings[,1], pc_loadings[,2])  
