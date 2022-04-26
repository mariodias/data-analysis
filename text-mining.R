library(stringr)
require(dplyr)
require(tidytext)
require(tm)
library(qdap)
library(ggplot2)
library(wordcloud2)


clean_tweets <- function(tweets) {
  
  mystopwords <- stopwords("portuguese")
  
  tweets$clean_text <- str_to_lower(tweets$tweet)
  tweets$clean_text <- stringi::stri_trans_general(tweets$clean_text,"Latin-ASCII")
  tweets$clean_text <-str_replace_all(tweets$clean_text,"@\\w+", " ")
  tweets$clean_text <-str_replace_all(tweets$clean_text,"#\\w+", " ")
  tweets$clean_text <- str_replace_all(tweets$clean_text,"http.+", "")
  tweets$clean_text <- str_remove_all(tweets$clean_text, "[[:punct:]]")
  tweets$clean_text <- str_replace_all(tweets$clean_text,"[[:digit:]]", "")
  tweets$clean_text <- str_replace_all(tweets$clean_text, "[[:emoji:]]", "")
  tweets$clean_text <- str_replace_all(tweets$clean_text,"([^rs])\\1+|(rr)(?=r+)|(ss)(?=s+)", "\\1")
  tweets$clean_text <- str_replace_all(tweets$clean_text,"\\W", " ")
  tweets$clean_text <- str_squish(tweets$clean_text)
  tweets$clean_text <- removeWords(tweets$clean_text, mystopwords)
  
  return(tweets)
  
}

text_stemmer <- function(tweets) {

  tweets$text_stemmer <- stemDocument(tweets$clean_text, language = "portuguese")
  
  tweetTokens <- tweets %>%
    unnest_tokens(term, clean_text)
  
  return(tweetTokens)
  
}



