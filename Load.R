# Milestone link
# http://rpubs.com/BDFace/93486

# Final Report slides link
# http://rpubs.com/BDFace/101638
# 
# Shiny App link
# http://bigdataface.shinyapps.io/Shiny

# Load the packages 
library(bitops); library(class); library(ggplot2); library(plyr); 
library(RCurl); library(rjson); library(stringr);
library(twitteR); library(XML); library(tm); library(RWeka); library(dplyr);
library(wordcloud); library(stringi); library(grid)

# Set directory
setwd("~/Desktop/Capstone/final/en_US")

# Load the data 
en_news <- as.matrix(readLines("en_US.news.txt",-1,skipNul = TRUE))
en_twit <- as.matrix(readLines("en_US.twitter.txt",-1,skipNul = TRUE))
en_blog <- as.matrix(readLines("en_US.blogs.txt",-1,skipNul = TRUE))

# Taking a sample of the data
en_blog_samp <- en_blog[1:100000,]
en_news_samp <- en_news[1:100000,]
en_twit_samp <- en_twit[1:100000,]

# Combining all the text together
text_blog <- paste(en_blog_samp, collapse=" ")
text_news <- paste(en_news_samp, collapse=" ")
text_twit <- paste(en_twit_samp, collapse=" ")

# Setting up source and Corpus
text_blog_cor <- VectorSource(text_blog)
text_news_cor <- VectorSource(text_news)
text_twit_cor <- VectorSource(text_twit)
cor_blog <- Corpus(text_blog_cor)
cor_news <- Corpus(text_news_cor)
cor_twit <- Corpus(text_twit_cor)

# Cleaning Corpus Blog
cor_blog <- tm_map(cor_blog, content_transformer(tolower))
cor_blog <- tm_map(cor_blog, removePunctuation)
cor_blog <- tm_map(cor_blog, stripWhitespace)
cor_blog <- tm_map(cor_blog, removeNumbers)

# Cleaning Corpus News
cor_news <- tm_map(cor_news, content_transformer(tolower))
cor_news <- tm_map(cor_news, removePunctuation)
cor_news <- tm_map(cor_news, stripWhitespace)
cor_news <- tm_map(cor_news, removeNumbers)

# Cleaning Corpus Twit
cor_twit <- tm_map(cor_twit, content_transformer(tolower))
cor_twit <- tm_map(cor_twit, removePunctuation)
cor_twit <- tm_map(cor_twit, stripWhitespace)
cor_twit <- tm_map(cor_twit, removeNumbers)

# Load the profanity list and use as stopwords
profan <- as.matrix(readLines("profanity_list.txt",-1,skipNul = TRUE))
profan <- paste(profan, collapse=" ")
cor_blog <- tm_map(cor_blog, removeWords, profan)
cor_news <- tm_map(cor_news, removeWords, profan)
cor_twit <- tm_map(cor_twit, removeWords, profan)

# Making a document-term matrix
dtm_blog <- DocumentTermMatrix(cor_blog)
dtm_blog2 <- as.matrix(dtm_blog)

dtm_news <- DocumentTermMatrix(cor_news)
dtm_news2 <- as.matrix(dtm_news)

dtm_twit <- DocumentTermMatrix(cor_twit)
dtm_twit2 <- as.matrix(dtm_twit)

# Term frequency. Blog
freq_blog <- colSums(dtm_blog2)
freq_blog <- sort(freq_blog, decreasing=TRUE)
freq_blog_col <- as.data.frame(freq_blog)
freq_blog_words <- names(freq_blog)
freq_blog_col$words <- freq_blog_words

# Wordcloud. Blog
words_blog <- names(freq_blog)
wordcloud(words_blog[1:100], freq_blog[1:100])

# Term frequency. News
freq_news <- colSums(dtm_news2)
freq_news <- sort(freq_blog, decreasing=TRUE)
freq_news_col <- as.data.frame(freq_news)
freq_news_words <- names(freq_news)
freq_news_col$words <- freq_news_words

# Wordcloud. News
words_news <- names(freq_news)
wordcloud(words_news[1:100], freq_news[1:100])

# Term frequency. Twit
freq_twit <- colSums(dtm_twit2)
freq_twit <- sort(freq_twit, decreasing=TRUE)
freq_twit_col <- as.data.frame(freq_twit)
freq_twit_words <- names(freq_twit)
freq_twit_col$words <- freq_twit_words

# Wordcloud. Twit
words_twit <- names(freq_twit)
wordcloud(words_twit[1:100], freq_twit[1:100])

# Qplot the frequency tables
plot_twit <- freq_twit_col[1:25,]
plot_news <- freq_news_col[1:25,]
plot_blog <- freq_blog_col[1:25,]
qplot(words, freq_twit, data=plot_twit, main="twitter")
qplot(words, freq_news, data=plot_news, main="news")
qplot(words, freq_blog, data=plot_blog, main="blog")




# Cutting down the n-grams where string appears more the once
pairs_cut <- pairs_col[pairs_col$pairs>=2,]
triplets_cut <- triplets_col[triplets_col$triplets >=2,]
quads_cut <- quads_col[quads_col$quads >=2,]
fives_cut <- fives_col[fives_col$fives >=2,]
sixes_cut <- sixes_col[sixes_col$sixes >=2,]


# Converting to character for sep
pairs_sep$pairs_tail <- as.character(pairs_sep$pairs_tail)
triplets_sep$triplets_tail <- as.character(triplets_sep$triplets_tail)
quads_sep$quads_tail <- as.character(quads_sep$quads_tail)
fives_sep$fives_tail <- as.character(fives_sep$fives_tail)
sixes_sep$sixes_tail <- as.character(sixes_sep$sixes_tail)

# Exporting the data into .rds
saveRDS(sixes_sep, "sixes_sep.rds")
saveRDS(fives_sep, "fives_sep.rds")
saveRDS(quads_sep, "quads_sep.rds")
saveRDS(triplets_sep, "triplets_sep.rds")
saveRDS(pairs_sep, "pairs_sep.rds")



# Combine the data sets
tiny_blog <- en_blog[1:10000,]
tiny_news <- en_news[1:10000,]
tiny_twit <- en_twit[1:10000,]
en_samp <- c(tiny_blog, tiny_news, tiny_twit)
en_samp_coll <- paste(en_samp, collapse=" ")
cor_samp_all <- VectorSource(en_samp_coll)
cor_all <- Corpus(cor_samp_all)
cor_all <- tm_map(cor_all, content_transformer(tolower))
cor_all <- tm_map(cor_all, removePunctuation)
cor_all <- tm_map(cor_all, stripWhitespace)
cor_all <- tm_map(cor_all, removeNumbers)

# 2 n-gram.
ngram_tok <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
ngram_tdm <- TermDocumentMatrix(cor_all, control=list(tokenize = ngram_tok))

# Extracting pairs
pairs <- as.matrix(ngram_tdm)
pairs <- sort(rowSums(pairs), decreasing = TRUE)
pairs_col <- as.data.frame(pairs)
pairs_words <- names(pairs)
pairs_col$pairs_words <- pairs_words
pairs_col_matrix <- as.matrix(pairs_col)

# 3 n-gram.
ngram_tok <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
ngram_tdm3 <- TermDocumentMatrix(cor_all, control=list(tokenize = ngram_tok))

# Extracting triplets
triplets <- as.matrix(ngram_tdm3)
triplets <- sort(rowSums(triplets), decreasing = TRUE)
triplets_col <- as.data.frame(triplets)
tri_words <- names(triplets)
triplets_col$tri_words <- tri_words
triplets_col_matrix <- as.matrix(triplets_col)

# 4 n-gram.
ngram_tok <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
ngram_tdm4 <- TermDocumentMatrix(cor_all, control=list(tokenize = ngram_tok))

# Extracting quads
quads <- as.matrix(ngram_tdm4)
quads <- sort(rowSums(quads), decreasing = TRUE)
quads_col <- as.data.frame(quads)
quads_words <- names(quads)
quads_col$quads_words <- quads_words
quads_col_matrix <- as.matrix(quads_col)

# 5 n-gram.
ngram_tok <- function(x) NGramTokenizer(x, Weka_control(min=5, max=5))
ngram_tdm5 <- TermDocumentMatrix(cor_all, control=list(tokenize = ngram_tok))

# Extracting quads
fives <- as.matrix(ngram_tdm5)
fives <- sort(rowSums(fives), decreasing = TRUE)
fives_col <- as.data.frame(fives)
fives_words <- names(fives)
fives_col$fives_words <- fives_words
fives_col_matrix <- as.matrix(fives_col)


# 6 n-gram.
ngram_tok <- function(x) NGramTokenizer(x, Weka_control(min=6, max=6))
ngram_tdm6 <- TermDocumentMatrix(cor_all, control=list(tokenize = ngram_tok))

# Extracting quads
sixes <- as.matrix(ngram_tdm6)
sixes <- sort(rowSums(sixes), decreasing = TRUE)
sixes_col <- as.data.frame(sixes)
sixes_words <- names(sixes)
sixes_col$sixes_words <- sixes_words
sixes_col_matrix <- as.matrix(sixes_col)








# Cutting down the user generated string by 1 where UG is the user input string
UG_1 <- gsub("^.*? "," ", UG)



# seperate the n-grams from the last word
pairs_tail <- word(pairs_col[,2], -1)
pairs_sep <- cbind(pairs_col, pairs_tail)

triplets_tail <- word(triplets_col[,2], -1)
triplets_sep <- cbind(triplets_col, triplets_tail)

quads_tail <- word(quads_col[,2], -1)
quads_sep <- cbind(quads_col, quads_tail)

fives_tail <- word(fives_col[,2], -1)
fives_sep <- cbind(fives_col, fives_tail)

sixes_tail <- word(sixes_col[,2], -1)
sixes_sep <- cbind(sixes_col, sixes_tail)










# Wordcloud
library(wordcloud)
words <- names(frequency)
wordcloud(words[1:100], frequency[1:100])

# Size of the files in megabytes
file.info("en_US.blogs.txt")$size / 1024^2

# Longest line in a text file
max(nchar(en_blog))
max(nchar(en_news))
max(nchar(en_twit))

# Number of times love appears/number of times hate appears
love_count <- sum(grepl("love", en_twit))
hate_count <- sum(grepl("hate", en_twit))
love_count / hate_count

# Finding and extracting specific words in tweets (^ indicates start of string)
great <- grep("^great", en_twit)
en_twit[great,]

# Finding exact matches
sum(grepl("A computer once beat me at", en_twit))

# inspect the 3 n-gram
data.frame(inspect(ngram_tdm3[1:100,]))

# Combining all the text together
text_all <- cbind(en_blog_samp, en_news_samp, en_twit_samp)
text_all <- paste(text_all, collapse=" ")

# Tidy up
rm(corpus, dtm, frequency, text_source, text_text, words, text, dtm2)

rm(dtm_blog2, dtm_news2, dtm_twit2, en_blog, en_news, en_twit, cor_blog, cor_news, cor_twit)
rm(dtm_blog, dtm_news, dtm_twit, en_blog_samp, en_news_samp, en_twit_samp, text_blog, text_news, text_twit)
rm(rmarkdownTable, profan, text_blog_cor, text_news_cor, text_twit_cor)