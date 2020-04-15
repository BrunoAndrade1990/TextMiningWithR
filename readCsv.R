

install.packages("stringr")
install.packages("plyr")
install.packages("tm")
install.packages("wordcloud")



cat("\014")
setwd("C:/ReadFile")
#View(comments)
#library(tm)
#install.packages("tm")
#install.packages("wordcloud")
#'stringr'
#library("sos")

#findFn("laply")

library(tm)
library(wordcloud)
library(plyr)
library(stringr)

csvFile <- read.csv(file="MIB - TextMining II.csv", header=TRUE, sep=",")
columnNames = colnames(csvFile)



# Create corpus
corpus=tm::Corpus(VectorSource(csvFile$Comment))

corpus=Corpus(VectorSource(corpus))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

#col=brewer.pal(6,"Dark2")
wordcloud::wordcloud(corpus, min.freq=5, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F)
wordcloud(corpus, max.words = 1000, random.order = FALSE)



#remove('positives','negatives','posi')
positives= readLines("positiveWords.txt")
negatives = readLines("negativeWords.txt")

score.sentiment = function(sentences, pos.words, neg.words
                           , .progress = 'none'){
  scores = laply(sentences, function(sentence, pos.words, neg.words){
    sentence = gsub("[[:punct:]]", "", sentence)
    sentence = gsub("[[:cntrl:]]", "", sentence)
    sentence = gsub("\\d+", "", sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, "\\s+")
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    ### we could have used "pos.matches = words %in% pos.words" to
    #get the TRUE/FALSE instead of using "match() and !is.na()"
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress = .progress )
  scores.df = data.frame(score = scores, text = sentences)
  return(scores.df)
}



#score = sentiment_scores(csvFile, word_list, word_listNeg)
listComment=csvFile$Comment
#View(csvFile)
score = score.sentiment(listComment, positives, negatives)
hist(score$score,
     main="Quantidade de comentários", 
     xlab="Comentários", 
     border="black", 
     col="grey",
     xlim=c(-1,4),
     #ylim =c(1,4),
     las=1, 
     breaks=8)

