library(tidytext)
library(tidyverse)
library(tidyr)

twitter_data <- read.csv("Final-Project/data/Tweets-1.csv")

twitter_tibble <- tibble(twitter_data)

twitter_unnested <- twitter_tibble %>%
  unnest_tokens(word,text)


twitter_sentiment <- twitter_unnested %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)


negative <- twitter_sentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "negative")

total_negative <- negative %>%
  count(sentiment, sort = TRUE)
  
positive <- twitter_sentiment %>%
  group_by(sentiment) %>%
  filter(sentiment == "positive")

total_positive <- positive %>%
  count(sentiment, sort = TRUE)

write.csv(twitter_sentiment, file = "twitter_sentiment.csv")
