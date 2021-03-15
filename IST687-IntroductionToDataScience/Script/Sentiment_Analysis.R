setwd("/Users/lvchaoying 1/Desktop/IST687/WEEK11/txt/")
library(tm)
library(NLP)
library(ggplot2)
#import positive words negative, positive words and stop words
posWords<-scan("positive-words.txt", character(0), sep = "\n")
negWords<-scan("negative-words.txt", character(0), sep = "\n")
stopWords<-scan("stopwords.txt", character(0), sep = "\n")
posWords<-posWords[-1:-34]
negWords<-negWords[-1:-34]
usefullwords<-c(posWords,negWords,stopWords)
#import the comment from costomers
freewords<-finalData$freeText
freewords<-freewords[freewords!=0]
freewords
text<-toString(freewords)
text
charVector <- scan("text.txt", character(0), sep = "\n")
write.table(x = text,file = "text.txt")
words.vec<-VectorSource(charVector)
words.crops<-Corpus(words.vec)
words.crops

words.crops<-tm_map(words.crops,content_transformer(tolower))
words.crops<-tm_map(words.crops,removePunctuation)
words.crops<-tm_map(words.crops,removeNumbers)
words.crops<-tm_map(words.crops,removeWords,stopwords("english"))
tdm<-TermDocumentMatrix(words.crops)
inspect(tdm)


m<-as.matrix(tdm)
wordCounts<-rowSums(m)
wordCounts <- sort(wordCounts, decreasing=TRUE)
wordCounts
#we have 7500 words.
sumWords<-sum(wordCounts)
sumWords
matchedP <- match(names(wordCounts), posWords, nomatch = 0) 
matchedP
matchedN<-match(names(wordCounts),negWords,nomatch = 0)
matchUse<-match(names(wordCounts),usefullwords, nomatch = 0)
matchUse
matchedN
matchStopwords<-match(names(wordCounts),stopWords,nomatch = 0)
length(matchedP) 
#we have 2091 posive wrords
Positive<- wordCounts[which(matchedP != 0)]
View(Positive)
naStopword<-wordCounts[which(matchStopwords == 0)]
naSentimentStopwords<-wordCounts[which(matchUse == 0)]
naSentimentStopwords
naStopword
Nagetive<-wordCounts[which(matchedN!=0)]
Nagetive
install.packages('wordcloud')
library(RColorBrewer)
library(wordcloud)
#The words cloud for positive word in free words.
wordcloud(names(Positive),Positive,min.freq = 2,max.words = 50,rot.per = 0.35,colors=brewer.pal(8,"Dark2"))
# The words cloud for nagetive word in free words.
wordcloud(names(Nagetive),Nagetive,min.freq = 2,max.words = 50,rot.per = 0.35,colors=brewer.pal(8,"Dark2"))
# The words cloud after delecting the stop words and sentiment words to find the words most mentioned from our costomers
wordcloud(names(naSentimentStopwords),naSentimentStopwords,min.freq = 2,max.words = 50,rot.per = 0.35,colors=brewer.pal(8,"Dark2"))
