setwd("F:/ML")
sms_raw <- read.csv('SMSSpamCollection.csv', stringsAsFactors = FALSE)
str(sms_raw)
# convert type to factor . we know it contains spam and ham classification

sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

str(sms_raw$text)
head(sms_raw$text)
colnames(sms_raw)
# convert the text column to character

library(dplyr)
sms_raw$text <-  as.character(sms_raw$text)

# the text column contains m\t and \t charcters which need to be removed

sms_raw$text <- gsub("[m\t]","",sms_raw$text)
sms_raw$text <- gsub("[\t]","",sms_raw$text)

head(sms_raw$text)

# load the text mining library

library(tm)

# load the raw text as a corpus , a collection of text documents

sms_corpus <- Corpus(VectorSource(sms_raw$text))
inspect(sms_corpus[1:3])

# setup clean corpus as below with respective arguements

Corpus_clean <- tm_map(sms_corpus, tolower)
Corpus_clean <- tm_map(Corpus_clean, removeNumbers)
Corpus_clean <- tm_map(Corpus_clean, removeWords, stopwords())
Corpus_clean <- tm_map(Corpus_clean, removePunctuation)
Corpus_clean <- tm_map(Corpus_clean, stripWhitespace)
inspect(Corpus_clean[1:3])

# new version of document matrix ( which creates a sparse matrix) needs an
# arguement to convert the data to a plain text document else gives an error

Corpus_clean <- tm_map(Corpus_clean, PlainTextDocument)
sms_dtm <- DocumentTermMatrix(Corpus_clean)

## split the data in raw clean and dtm to same ratios

n = nrow(sms_raw)
smsindex = sample(1:n, size = round(0.7*n), replace = FALSE)
sms_raw_train = sms_raw[smsindex, ]
sms_raw_test = sms_raw[-smsindex, ]

sms_dtm_train = sms_dtm[smsindex, ]
sms_dtm_test = sms_dtm[-smsindex, ]

sms_corpus_train <- Corpus_clean[1:3902]
sms_corpus_test <- Corpus_clean[3903:5574]

# install.packages('wordcloud')
library(wordcloud)
#wordcloud(sms_corpus_train,
#          min.freq = 40, random.order = FALSE,
#          scale = c(4,1), max.words = 300,
#          colors = 1)

wordcloud(sms_corpus_train,
          min.freq = 40, random.order = FALSE,
           max.words = 200,
          colors = c(1:9))
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3,0.5))
wordcloud(ham$text, max.words = 40, scale = c(3,0.5))

# from the sparse matrix list out the words which occur in  atleast 5 messages
# dictionary () is not longer supported in TM, use terms()

sms_dict <- c(findFreqTerms(sms_dtm_train, 5))

head (sms_dict)


sms_train <- DocumentTermMatrix(sms_corpus_train, list (sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list (sms_dict))

convert_counts <- function(x) {
  x <- ifelse(x>0, 1,0)
  x<- factor(x,levels = c(1,0), labels = c("NO","YES"))
  }

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

# install.packages("e1071")
# install.packages("klaR")
library(klaR)

sms_classf <- NaiveBayes(sms_train, sms_raw_train$type, laplace =1)
sms_test_pred <- predict(sms_classf, sms_test)

# View (sms_test_pred)

# install.packages('gmodels')
library(gmodels)

CrossTable(sms_test_pred$class, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))
