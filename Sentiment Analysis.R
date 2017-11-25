##Assignment 2

setwd("D:/Omis 670")
getwd()

## installing all the packages ---------------------------------------------------

install.packages("topicmodels")
install.packages("ldatuning")
install.packages("tidytext")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
install.packages("rvest")
install.packages("tm")
install.packages("plyr")

## ----------------------------------------------------------------loading the libraries --------------------------------------------------------

library(plyr)
library(rvest)
library(tm)
library(tidytext)
library(dplyr)
library(ldatuning)
library(stringr)
library(topicmodels)
library(tidyr)

##-----------------------------------------------------------building a lexicon file dictionary ----------------------------------------------


url<-"https://www.powerthesaurus.org/anger/synonyms/"
A <- NULL
N_pages<-72
D<-NULL
for (j in 2: N_pages){
  pixel <- read_html(paste0(url, j)) 
  B <- cbind(pixel %>% html_nodes(".topic-link span , .topic-link") %>%     html_text()     )
  A <- rbind(A,B)
}

write(A, file="angry_words.txt")



## ---------------------------------Applying the sentiment analysis approach to post gathered from Facebook-----------------------------------------------

#GetPage- Extract list of posts from a public Facebook page
page<-getPage(page="Equifax", token=fb_oauth, n = 10,  feed = FALSE, 
              reactions = TRUE,  verbose = TRUE, api = NULL)
#page<-getCommentReplies(comment_id =def$text[20], token = fb_oauth, n=1000)

#getPost-Extract information about a public Facebook post - Returns a list
post<-getPost(post = page$id[4], n=1000,token=fb_oauth)
post[3]
post.df<-as.data.frame(post[3])

post1.df <- sapply(post.df$comments.message, function(x) iconv(x, to='UTF-8', sub='byte'))
post1.df<-as.data.frame(post1.df)
colnames(post1.df)[1]<-"text"

anger<-readLines("angry_words.txt")


score.sentiment<-function(sentences, anger.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores<-laply(sentences,
                function(sentence, anger.words)
                {
                  # remove punctuation
                  sentence<-gsub("[[:punct:]]", "", sentence)
                  # remove control characters
                  sentence<-gsub("[[:cntrl:]]", "", sentence)
                  # remove digits?
                  sentence<-gsub('\\d+', '', sentence)
                  
                  #convert to lower
                  sentence<-tolower(sentence)
                  #remove numbers
                  sentence<-removeNumbers(sentence)
                  
                  
                  # split sentence into words with str_split (stringr package)
                  word.list<- str_split(sentence, "\\s+")
                  words<- unlist(word.list)
                  
                  # compare words to the dictionaries of positive & negative terms
                  anger.matches<-match(words, anger)
                  
                  # get the position of the matched term or NA
                  # we just want a TRUE/FALSE
                  anger.matches<- !is.na(anger.matches)
                  
                  # final score
                  score<- sum(anger.matches) 
                  return(score)
                }, anger.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df<- data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores_facebook<-score.sentiment(post1.df$text, anger, .progress='text')

#Convert sentiment scores from numeric to character to enable the gsub function 
scores_facebook$score_chr<-as.character(scores_facebook$score)

#After looking at the summary(scores_facebook$score) decide on a threshold for the sentiment labels
scores_facebook$score_chr<-gsub("^0$", "Neutral", scores_facebook$score_chr)
scores_facebook$score_chr<-gsub("^1$|^2$|^3$|^4$|^5$|^6$", "Angry", scores_facebook$score_chr)
scores_facebook$score_chr<-gsub("^7$|^8$|^9$|^10$|^11$|^12$", "Very Angry", scores_facebook$score_chr)

scores_facebook$score_chr<-gsub("^13$|^14$|^15|^16$|^17$|^18$|^19$|^20$", "Lost it", scores_facebook$score_chr)

View(scores_facebook)

scores_facebook%>% group_by(score)%>%summarise(MeanScore=mean(scores_facebook$score),MedianScore=median(scores_facebook$score))


max(scores_facebook$score)
min(scores_facebook$score)

















