library(rtweet)
library(dplyr)
library(tokenizers)
library(tidytext)

#establishing API connection
twitter_token <- create_token(
  app = project_name,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret,
  set_renv = TRUE)

corona_virus <- search_tweets("coronavirus", n=1000, include_rts=FALSE, lang="en")

corona_virus$text[1:5]

# text cleaning
corona_virus$text <-  gsub("https\\S*", "", corona_virus$text)
corona_virus$text <-  gsub("@\\S*", "", corona_virus$text) 
corona_virus$text  <-  gsub("amp", "", corona_virus$text) 
corona_virus$text  <-  gsub("[\r\n]", "", corona_virus$text)
corona_virus$text  <-  gsub("[[:punct:]]", "", corona_virus$text)

# tokenization
tweets <- corona_virus %>%
  select(text) %>%
  unnest_tokens(word, text)
tweets <- tweets %>%
  anti_join(stop_words)

# creating term document matrix
words <- tweets %>% count(word, sort=TRUE)
words <- words[-which(words$word == "coronavirus"),]

wordcloud(words=words$word,freq = words$n, min.freq=3, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
