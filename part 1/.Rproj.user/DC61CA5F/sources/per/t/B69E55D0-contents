# Part I

library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(syuzhet)
library(SnowballC)

travel_review.df <- read.csv("traveler_reviews.csv", stringsAsFactors = FALSE)
thailand_review <- travel_review.df[travel_review.df$Country=="Thailand", ]
malaysia_review <- travel_review.df[travel_review.df$Country=="Malaysia", ]
japan_review <- travel_review.df[travel_review.df$Country=="Japan", ]

#Thailand
thailand_corpus <- Corpus(VectorSource(thailand_review$ReviewText))
thailand_corpus <- tm_map(thailand_corpus, content_transformer(tolower))
thailand_corpus <- tm_map(thailand_corpus, removePunctuation)
thailand_corpus <- tm_map(thailand_corpus, removeWords, c(stopwords("en"), "bampb", "etc", "amp" ) )
wordcloud(thailand_corpus, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

#Malaysia
malaysia_corpus <- Corpus(VectorSource(malaysia_review$ReviewText))
malaysia_corpus <- tm_map(malaysia_corpus, content_transformer(tolower))
malaysia_corpus <- tm_map(malaysia_corpus, removePunctuation)
malaysia_corpus <- tm_map(malaysia_corpus, removeWords, c(stopwords("en"), "bampb", "’d", "etc", "amp" ) )
wordcloud(malaysia_corpus, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))                        

#Japan
japan_corpus <- Corpus(VectorSource(japan_review$ReviewText))
japan_corpus <- tm_map(japan_corpus, content_transformer(tolower))
japan_corpus <- tm_map(japan_corpus, removePunctuation)
japan_corpus <- tm_map(japan_corpus, removeWords, c(stopwords("en"), "bampb", "etc", "amp" ) )
wordcloud(japan_corpus, max.words = 150, random.order = FALSE, colors = brewer.pal(8, "Dark2"))
                          
#Thailand Dominant Sentiment
thailand_sentiments <- get_nrc_sentiment(thailand_review$ReviewText)
dominant_thailand_sentiment <- names(which.max(colSums(thailand_sentiments)))
print(paste("The dominant sentiment for Thailand is:", dominant_thailand_sentiment))

#Malaysia Dominant Sentiment
malaysia_sentiments <- get_nrc_sentiment(malaysia_review$ReviewText)
dominant_malaysia_sentiment <- names(which.max(colSums(malaysia_sentiments)))
print(paste("The dominant sentiment for Malaysia is:", dominant_malaysia_sentiment))

#Japan Dominant Sentiment
japan_sentiments <- get_nrc_sentiment(japan_review$ReviewText)
dominant_japan_sentiment <- names(which.max(colSums(japan_sentiments)))
print(paste("The dominant sentiment for Japan is:", dominant_japan_sentiment))

