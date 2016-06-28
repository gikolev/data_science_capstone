## Set working directory
setwd("/Users/gik/Data_Analytics/R_Directory/coursera/Data_Science_Capstone/Exploratory_Analysis")

## Read blogs, news and twitter files into character vectors
blogs <- readLines("en_US.blogs.txt", encoding = "UTF-8", skipNul = TRUE)
news <- readLines("en_US.news.txt", encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt", encoding = "UTF-8", skipNul = TRUE)

## Create test (10%) and train (90%) character vectors
set.seed(12345)
index <- sample(1:length(blogs), size = 0.1*length(blogs))
blogs_train <- blogs[index]
blogs_test <- blogs[-index]
index <- sample(1:length(news), size = 0.1*length(news))
news_train <- news[index]
news_test <- news[-index]
index <- sample(1:length(twitter), size = 0.1*length(twitter))
twitter_train <- twitter[index]
twitter_test <- twitter[-index]

## Create corpus
library(quanteda)
corpus_train <- corpus(c(blogs_train, news_train, twitter_train))

## Create list of profane words to filter out
profanity <- readLines("profanity_list_en.txt", encoding = "UTF-8", skipNul = TRUE)

## Create 1 through 4-gram document frequency matrices and extract frequency tables as data frames 
## and create separate columns for each word in the n-gram; remove n-grams occurring only once.
df_list <- list()
library(stringr)
library(dplyr)
strt <- Sys.time()
for(i in 1:4) {
        x <- dfm(corpus_train,
                 ngrams = i,
                 toLower = TRUE, 
                 removeNumbers = TRUE, 
                 removePunct = TRUE, 
                 removeSeparators = TRUE, 
                 ignoredFeatures = profanity, 
                 stem = FALSE,
                 verbose = FALSE)
        data <- as.data.frame(topfeatures(x, n = length(x)))
        colnames(data) <- "freq"
        data$ngram <- row.names(data)
        row.names(data) <- c(1:nrow(data))
        data$rank <- c(1:nrow(data))
        data$pct_total <- data$freq / sum(data$freq) * 100 
        data$pct_cumul <- cumsum(data$pct_total)
        data <- filter(data, freq > 1)
        df_list[[i]] <- data
        df_list[[i]] <- cbind(df_list[[i]],
                              as.data.frame(str_split_fixed(df_list[[i]]$ngram, "_", i)))
}
print(Sys.time()-strt)

## Assign character class to word columns
df_list[[1]]$V1 <- as.character(df_list[[1]]$V1)
df_list[[2]]$V1 <- as.character(df_list[[2]]$V1)
df_list[[2]]$V2 <- as.character(df_list[[2]]$V2)
df_list[[3]]$V1 <- as.character(df_list[[3]]$V1)
df_list[[3]]$V2 <- as.character(df_list[[3]]$V2)
df_list[[3]]$V3 <- as.character(df_list[[3]]$V3)
df_list[[4]]$V1 <- as.character(df_list[[4]]$V1)
df_list[[4]]$V2 <- as.character(df_list[[4]]$V2)
df_list[[4]]$V3 <- as.character(df_list[[4]]$V3)
df_list[[4]]$V4 <- as.character(df_list[[4]]$V4)

## Give names to the frequency tables
names(df_list) <- c("UnigramFreqTable",
                    "BigramFreqTable",
                    "TrigramFreqTable",
                    "QuadrigramFreqTable")

## Create predict function that takes text as input and predicts the next word
predict <- function(x) {
        y <- strsplit(tolower(x), " ")[[1]]
        n <- length(y)
        if (n == 1) {
                q <- which(df_list[[2]]$V1 == y)
                print(df_list[[2]][min(q), "V2"])
        } else if (n == 2) {
                q <- which(df_list[[3]]$V1 == y[1]
                           & df_list[[3]]$V2 == y[2])
                if (length(q) == 0) {
                        q <- which(df_list[[2]]$V1 == y[2])
                        print(df_list[[2]][min(q), "V2"])
                } else {
                        print(df_list[[3]][min(q), "V3"])
                }
        } else if (n >= 3) {
                z <- tail(y, n = 3) 
                q <- which(df_list[[4]]$V1 == z[1] 
                           & df_list[[4]]$V2 == z[2] 
                           & df_list[[4]]$V3 == z[3])
                if (length(q) == 0) {
                        q <- which(df_list[[3]]$V1 == z[2]
                                   & df_list[[3]]$V2 == z[3])
                        if (is.null(q)) {
                                q <- which(df_list[[2]]$V1 == z[3])
                                print(df_list[[2]][min(q), "V2"])
                        } else {
                                print(df_list[[3]][min(q), "V3"])
                        }
                } else {
                        print(df_list[[4]][min(q), "V4"])
                }
        }
}

## Test predict function
predict("I would like a cup of")
predict("Nice to meet")
predict("Crate and")
predict("Peanut")

