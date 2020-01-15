# load library
library(rtweet)
# another library you can use: twitteR

# get tweets
iran_tweets <- search_tweets('iran', type = "recent", n = 5000, 
                             lang='en', include_rts = FALSE)

# how many tweets did we get?
nrow(iran_tweets)

# check data
View(iran_tweets)

# https://www.tidytextmining.com/
library(tidyverse)
library(tidytext)


# tokenize tweets
tokenized_tweets <- iran_tweets %>% 
  unnest_tokens(word, text)

# what size is this corpus?
nrow(tokenized_tweets)

### remove stopwords
# what are stopwords?
stopwords <- get_stopwords()
stopwords$word
# use anti_join to remove stopwords
cleaned_tokenized_tweets <- tokenized_tweets %>%
  anti_join(stopwords)
# what is the size of our corpus now?
nrow(cleaned_tokenized_tweets)

# count words that are left
word_count <- cleaned_tokenized_tweets %>%
  count(word, sort = TRUE) 

View(word_count)

# do some more cleaning
words2remove <- c('https', 't.co', 'amp')
cleaned_tokenized_tweets_v2 <- cleaned_tokenized_tweets %>%
  filter(!(word %in% words2remove))

# count words that are left
word_count <- cleaned_tokenized_tweets_v2 %>%
  count(word, sort = TRUE) 

View(word_count)

## collocations with "iran"
# get bigrams
# tokenize tweets
tweet_bigrams <- iran_tweets %>% 
  select(screen_name, text) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# separate the words in the bigram
bigrams_separated <- tweet_bigrams %>%
  separate(bigram, c('word1', 'word2'), sep = ' ')

# remove stopwords and other words
words2remove <- c(stopwords$word, 'https', 't.co', 'fwkwuc9ooe', 'amp')

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% words2remove &
           !word2 %in% words2remove)
  
# count collocations with "iran"
iran_collocation_counts <- bigrams_filtered %>%
  filter(word1 == "iran" | word2 == 'iran') %>%
  count(word1, word2, sort = TRUE)

## Compare Tweets by Geolocation
# get tweets for new york, "latitude,longitude,radius" 
iran_tweets_ny <- search_tweets('iran', type = "recent", n = 5000, lang='en',
                                geocode = "40.730610,-73.935242,100mi", 
                                include_rts = FALSE)

nrow(iran_tweets_ny)

iran_tweets_az <- search_tweets('iran', type = "recent", n = 5000, lang='en',
                                geocode = "33.448376,-112.074036,100mi", 
                                include_rts = FALSE)
nrow(iran_tweets_az)

# remove urls from text
iran_tweets_az$clean_text <- gsub("http.*","",  iran_tweets_az$text)
iran_tweets_az$clean_text <- gsub("@\\w+","",  iran_tweets_az$clean_text)
iran_tweets_az$clean_text <- gsub("#\\w+","",  iran_tweets_az$clean_text)
iran_tweets_az$clean_text <- gsub("&\\w+","",  iran_tweets_az$clean_text)
View(iran_tweets_az %>%
  select(clean_text, text))

iran_tweets_ny$clean_text <- gsub("http.*","",  iran_tweets_ny$text)
iran_tweets_ny$clean_text <- gsub("@\\w+","",  iran_tweets_ny$clean_text)
iran_tweets_ny$clean_text <- gsub("#\\w+","",  iran_tweets_ny$clean_text)
iran_tweets_ny$clean_text <- gsub("&\\w+","",  iran_tweets_ny$clean_text)

# add our location to each data frame so we don't lose that info later
iran_tweets_ny$target_location <- 'new york'
iran_tweets_az$target_location <- 'arizona'

# combine data frames
iran_tweets_comparison <- bind_rows(iran_tweets_ny,
                                    iran_tweets_az)
# select only text and location
iran_tweets_comparison <- iran_tweets_comparison %>%
  select(clean_text, target_location)

# tokenize tweets
tokenized_tweet_comparison <- iran_tweets_comparison %>% 
  unnest_tokens(word, clean_text)

# remove stopwords
cleaned_tokenized_tweet_comparison <- tokenized_tweet_comparison %>%
  anti_join(stopwords)

# count words per location
word_count_per_location <- cleaned_tokenized_tweet_comparison %>%
  group_by(target_location, word) %>%
  summarise(n = n()) %>%
  mutate(total = sum(n),
         norm_freq = n/total) %>%
  arrange(-n)

# plot most frequent words for each location
word_count_per_location  %>%
  filter(norm_freq > .003) %>%
  ggplot(aes(x = reorder(word, norm_freq), 
             y = norm_freq,
             fill = target_location)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  facet_wrap(~target_location, scales = "free")
  

# TF-IDF term frequency–inverse document frequency
# tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common
tf_idf_per_location <- word_count_per_location %>%
  bind_tf_idf(word, target_location, n)

tf_idf_per_location %>%
  select(-total) %>%
  arrange(desc(tf_idf))

tf_idf_per_location %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(target_location) %>% 
  top_n(30) %>% 
  ungroup() %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = target_location)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~target_location, ncol = 2, scales = "free") +
  coord_flip()

# more resources
# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/
# https://datacrunchcorp.com/podcast/social-media-and-machine-learning/
