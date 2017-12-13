library(tidyverse)
library(tidytext)
library(lexiconPT)
library(ggplot2)
library(stopwords)
library(tidyr)
library(readxl)
library(stats)
# Read in the twitter data
data <- read_excel("data.xlsx")

# Take a look at what class each column.  May need to change the
# class of some columns to work with them in meaningful ways.
glimpse(data)

# Notes:  Klout.Score will need to be converted to numeric.
#         Gender will need to be converted to factor.

# Arrange tweets in chronological order
data <- data %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(data$State) # 28 unique regions
unique(data$City) # 288 unique cities

# Exploring distribution of klout score
data$Klout.Score <- as.numeric(data$Klout.Score)
hist(data$Klout.Score)
summary(data$Klout.Score)

# Exploring the distribution of the number of followers
hist(data$Followers)
summary(data$Followers)
# Highly skewed to the right.

# Exploring the distribution of the number following
hist(data$Following)
summary(data$Following)
# Highly skewed to the right

# Exlporing the distribution of the number of posts
hist(data$Posts)
summary(data$Posts)
# Highly skewed to the right

# Exlporing Gender
data$Gender <- as.factor(data$Gender)
table(data$Gender)

# Exloring relationships across Klout.Score and Followers, Following, Posts

# Applying log transformations to Followers, Following, and Posts.
data <- data %>%
  mutate(logFollowers = log(Followers + 1), # adding one for zero followers
         logFollowing = log(Following + 1), # adding one for zero following
         logPosts = log(Posts + 1)) # adding one for zero posts

hist(data$logFollowers) # Appears normal
hist(data$logFollowing) # Appears normal
hist(data$logPosts) # Appears normal, a little skewed to the left

plot(data$logFollowers, data$Klout.Score)
plot(data$logFollowing, data$Klout.Score)
plot(data$logPosts, data$Klout.Score)

##############################################################################
# N-GRAMS 

# Finding the most frequent one-, two-, and three-word tokens over all tweets
# and filtered by brand/product
##############################################################################

##############################################################################
# One-Word Tokens over all tweets
contents <- data %>%
  mutate(line = row_number(), text = Contents)

contents

contents_word <- contents %>%
  unnest_tokens(word, text)

lexicon_pt <- oplexicon_v2.1 %>%
  mutate(word = term)

# Read in the portuguese stopwords
stop_pt <- data_frame(word = stopwords$pt)
stop_pt <- filter(stop_pt, word != "com")

stop_en <- data_frame(word = stopwords$en)
# Plotting the most frequent used words over all tweets

n_plot <- contents_word %>%
  dplyr::anti_join(stop_pt) %>%
  dplyr::anti_join(stop_en) %>%
  dplyr::count(word) %>%
  top_n(15) %>%
  dplyr::mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Most Frequent Words Over All Data") +
  ylab("word count") +
  xlab("one-word token") +
  theme(plot.title = element_text(hjust = .5))

n_plot # looks like some stopwords got through

##############################################################################
# Tokenizing by N-gram - Entire Data Set

##############################################################################
# bigrams

contents_bigrams <- contents %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

# Among the most popular bigrams contain stopwords.  Lets remove those using
# tidyr's separate().  This will split the bigram into two columns containing
# the first word of the bigram and the second word of the bigram.  Then we
# can filter out the stopwords that are part of the bigram and fine 'filtered'
# bigrams without the stopwords.

bigrams_separated <- contents_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_pt$word) %>%
  filter(!word2 %in% stop_pt$word) %>%
  filter(!word1 %in% stop_en$word) %>%
  filter(!word2 %in% stop_en$word)

# new bigram counts
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

# Now lets create the new bigrams using the filtered bigrams from above
# which do not contain stopwords.  Using tidyr's unite().
# Note that bigrams are in chronological order identified by the 'tweet id'
# from the original data sorted by date

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ")

n_plot_bigrams <- bigrams_united %>%
  # anti_join(stop_pt) %>%
  count(bigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab("bigram") +
  ylab("word count") +
  ggtitle("Most Frequent Bigrams Over All Data") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()
  

n_plot_bigrams

##############################################################################
# trigrams

contents_trigrams <- contents %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word3 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word,
         !word3 %in% stop_en$word)


trigrams_united <- contents_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

n_plot_trigrams <- trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  xlab("trigram") +
  ylab("word count") +
  ggtitle("Most Frequent Trigrams Over All Data") +
  theme(plot.title = element_text(hjust = .5)) 

n_plot_trigrams

##############################################################################
# Tokenizing by N-gram - Filtered Data by brand and type

fireball <- read_excel("fireball.xlsx")
jackfire <- read_excel("jackfire.xlsx")
jackhoney <- read_excel("jackhoney.xlsx")
jager <- read_excel("jager.xlsx")

# Transforming Followers, Following, and Posts to logs for filtered data sets
fireball <- fireball %>%
  mutate(logFollowers = log(Followers + 1), # adding one for zero followers
         logFollowing = log(Following + 1), # adding one for zero following
         logPosts = log(Posts + 1)) # adding one for zero posts

jackfire <- jackfire %>%
  mutate(logFollowers = log(Followers + 1), # adding one for zero followers
         logFollowing = log(Following + 1), # adding one for zero following
         logPosts = log(Posts + 1)) # adding one for zero posts

jackhoney <- jackhoney %>%
  mutate(logFollowers = log(Followers + 1), # adding one for zero followers
         logFollowing = log(Following + 1), # adding one for zero following
         logPosts = log(Posts + 1)) # adding one for zero posts

jager <- jager %>%
  mutate(logFollowers = log(Followers + 1), # adding one for zero followers
         logFollowing = log(Following + 1), # adding one for zero following
         logPosts = log(Posts + 1)) # adding one for zero posts

##############################################################################
# Jack Honey N-Grams
##############################################################################

# Arrange tweets in chronological order
jackhoney <- jackhoney %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(jackhoney$State) # 28 unique regions
unique(jackhoney$City) # 288 unique cities

# Exploring distribution of klout score
jackhoney$Klout.Score <- as.numeric(jackhoney$Klout.Score)
hist(jackhoney$Klout.Score)
summary(jackhoney$Klout.Score)

contents_jackhoney <- jackhoney %>%
  mutate(line = row_number(), text = Contents) %>%
  arrange(Category, Date)

contents_jackhoney

jackhoney_word <- contents_jackhoney %>%
  unnest_tokens(word, text)

# jackhoney unigram

jackhoney_n_plot <- jackhoney_word %>%
  anti_join(stop_pt) %>%
  anti_join(stop_en) %>%
  count(word, sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("one-word token") +
  ylab("word count") +
  ggtitle("Jack Honey Most Frequent Words") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jackhoney_n_plot # looks like some stopwords got through

# jackhoney bigrams

jackhoney_bigrams <- contents_jackhoney %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word)

jackhoney_bigrams_united <- jackhoney_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

jackhoney_n_plot_bigrams <- jackhoney_bigrams_united %>%
  # anti_join(stop_pt) %>%
  count(bigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("bigram token") +
  ylab("word count") +
  ggtitle("Jack Honey Most Frequent Bigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jackhoney_n_plot_bigrams

# jackhoney trigrams

jackhoney_trigrams <- contents_jackhoney %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word3 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word,
         !word3 %in% stop_en$word)


jackhoney_trigrams_united <- jackhoney_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

jackhoney_n_plot_trigrams <- jackhoney_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("trigram token") +
  ylab("word count") +
  ggtitle("Jack Honey Most Frequent Trigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jackhoney_n_plot_trigrams


########################################################################################
# Jager N-Grams
########################################################################################

# Create a dataframe of the tweets, which has a column name of 'Contents'
contents_jager <- jager %>%
  mutate(line = row_number(), text = Contents) %>%
  arrange(Category, Date)

jager_word <- contents_jager %>%
  unnest_tokens(word, text)

# Jager unigram

jager_n_plot <- jager_word %>%
  anti_join(stop_pt) %>%
  anti_join(stop_en) %>%
  dplyr::count(word, sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("one-word token") +
  ylab("word count") +
  ggtitle("Jager Most Frequent Words") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jager_n_plot # looks like some stopwords got through

# jager bigrams

jager_bigrams <- contents_jager %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word)

jager_bigrams_united <- jager_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

jager_n_plot_bigrams <- jager_bigrams_united %>%
  # anti_join(stop_pt) %>%
  dplyr::count(bigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("bigram") +
  ylab("word count") +
  ggtitle("Jager Most Frequent Bigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jager_n_plot_bigrams


# jager trigrams


jager_trigrams <- contents_jager %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word3 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word,
         !word3 %in% stop_en$word)


jager_trigrams_united <- jager_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

jager_n_plot_trigrams <- jager_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("trigram") +
  ylab("word count") +
  ggtitle("Jager Most Frequent Trigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jager_n_plot_trigrams


##############################################################################
# Fireball N-Grams
##############################################################################

# Arrange tweets in chronological order
fireball <- fireball %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(fireball$State) # 28 unique regions
unique(fireball$City) # 288 unique cities

# Exploring distribution of klout score
fireball$Klout.Score <- as.numeric(fireball$Klout.Score)
hist(fireball$Klout.Score)
summary(fireball$Klout.Score)

contents_fireball <- fireball %>%
  mutate(line = row_number(), text = Contents)

contents_fireball

fireball_word <- contents_fireball %>%
  unnest_tokens(word, text)

# Fireball unigram

fireball_n_plot <- fireball_word %>%
  anti_join(stop_pt) %>%
  anti_join(stop_en) %>%
  count(word, sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("one-word token") +
  ylab("word count") +
  ggtitle("Fireball Most Frequent Words") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

fireball_n_plot # looks like some stopwords got through

# fireball bigrams

fireball_bigrams <- contents_fireball %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word)

fireball_bigrams_united <- fireball_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

fireball_n_plot_bigrams <- fireball_bigrams_united %>%
  # anti_join(stop_pt) %>%
  count(bigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("bigram") +
  ylab("word count") +
  ggtitle("Fireball Most Frequent Bigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

fireball_n_plot_bigrams # notice the amount of english words!

# fireball trigrams

fireball_trigrams <- contents_fireball %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word3 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word,
         !word3 %in% stop_en$word)


fireball_trigrams_united <- fireball_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

fireball_n_plot_trigrams <- fireball_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  top_n(15) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("trigram") +
  ylab("word count") +
  ggtitle("Fireball Most Frequent Trigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

fireball_n_plot_trigrams  


##############################################################################
# jackfire
##############################################################################

# Arrange tweets in chronological order
jackfire <- jackfire %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(jackfire$State) # 28 unique regions
unique(jackfire$City) # 288 unique cities

# Exploring distribution of klout score
jackfire$Klout.Score <- as.numeric(jackfire$Klout.Score)
hist(jackfire$Klout.Score)
summary(jackfire$Klout.Score)

# Create a dataframe of the tweets only, which has a column name of 'Contents'
# Grouping by category here

contents_jackfire <- jackfire %>%
  mutate(line = row_number(), text = Contents) %>%
  arrange(Category, Date)

contents_jackfire

jackfire_word <- contents_jackfire %>%
  unnest_tokens(word, text)

jackfire_n_plot <- jackfire_word %>%
  anti_join(stop_pt) %>%
  anti_join(stop_en) %>%
  count(word, sort=TRUE) %>%
  top_n(15) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("one-word token") +
  ylab("word count") +
  ggtitle("Jack Fire Most Frequent Words") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jackfire_n_plot # looks like some stopwords got through

# jackfire bigrams

jackfire_bigrams <- contents_jackfire %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word)

jackfire_bigrams_united <- jackfire_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

jackfire_n_plot_bigrams <- jackfire_bigrams_united %>%
  # anti_join(stop_pt) %>%
  count(bigram, sort=TRUE) %>%
  top_n(10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("bigram") +
  ylab("word count") +
  ggtitle("Jack Fire Most Frequent Bigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jackfire_n_plot_bigrams 

##############################################################################
# Analyzing bigrams - test script for graphing bigrams

library(igraph)
jackfire_bigrams_count <- jackfire_bigrams %>%
  count(word1, word2, sort = TRUE)

jackfire_bigram_graph <- jackfire_bigrams_count %>%
  filter(n > 2) %>%
  graph_from_data_frame()


library(ggraph)
set.seed(2017)

ggraph(jackfire_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
##############################################################################

# jackfire trigrams

jackfire_trigrams <- contents_jackfire %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word3 %in% stop_pt$word,
         !word1 %in% stop_en$word,
         !word2 %in% stop_en$word,
         !word3 %in% stop_en$word)


jackfire_trigrams_united <- jackfire_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

jackfire_n_plot_trigrams <- jackfire_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  top_n(5) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("trigram") +
  ylab("word count") +
  ggtitle("Jack Fire Most Frequent Trigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

jackfire_n_plot_trigrams  

##############################################################################
##############################################################################
# Sentiment Analysis
##############################################################################
##############################################################################

# All tweets data
##############################################################################

# Giving a sentiment score for each tweet 
sentiment_score <- contents_word %>%
  anti_join(stop_pt) %>%
  anti_join(stop_en) %>%
  inner_join(lexicon_pt) %>%
  group_by(line) %>%
  summarise(sentiment_tweet = sum(polarity))

contents_with_score <- merge(sentiment_score, contents, by = "line")
  
# Exploring distribution of sentiment for each tweet
hist(contents_with_score$sentiment_tweet)
summary(contents_with_score$sentiment_tweet)

# Exploring Relationship between sentiment of the tweet and number of
# followers, following, and posts.

cor.test(contents_with_score$sentiment_tweet, contents_with_score$logFollowers)
# Results: r = 0.016, p = 0.068
# extremely weak positive relationship between sentiment and Followers
cor.test(contents_with_score$sentiment_tweet, contents_with_score$logFollowing)
# Results: r = -0.022, p = 0.013
# extremely weak negative relationship between sentiment and Following
cor.test(contents_with_score$sentiment_tweet, contents_with_score$logPosts)
# Results: r = -0.028, p = 0.0014
# extremely weak negative relationhsip between sentiment and Posts
cor.test(contents_with_score$sentiment_tweet, contents_with_score$Klout.Score)
# Results: r = -0.025, p = 0.005
# extremely weak negative relationship between Klout Score and sentiment 

# Average sentiment by Gender for all tweets
sentiment_gender <- contents_with_score %>%
  group_by(Gender) %>%
  summarise(sentiment_gender = mean(sentiment_tweet))

# Average sentiment by Category for all tweets
sentiment_category <- contents_with_score %>%
  group_by(Category) %>%
  summarise(sentiment_category = mean(sentiment_tweet))

##############################################################################
# Jack Honey Sentiment Analysis
##############################################################################
    # Giving a sentiment score for each Jack Honey Tweet
    ## Gives polarity of positive, negative, or neutral for the tweet as a whole
    jh_sentiment_score <- jackhoney_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      group_by(line) %>%
      summarise(jh_sentiment_tweet = sum(polarity))


    # Gives sentiment score as a continuous variable
    jh_with_score <- merge(jh_sentiment_score, 
                                  contents_jackhoney, by = "line")

    # Exploring distribution of sentiment for each tweet
    hist(jh_with_score$jh_sentiment_tweet)
    
    
    summary(jh_with_score$jh_sentiment_tweet)
    
    # Exploring Relationship between sentiment of the tweet and number of
    # followers, following, and posts.
    
    cor.test(jh_with_score$jh_sentiment_tweet, jh_with_score$logFollowers)
    # No Relationship
    
    cor.test(jh_with_score$jh_sentiment_tweet, jh_with_score$logFollowing)
    # No Relationship
    
    cor.test(jh_with_score$jh_sentiment_tweet, jh_with_score$logPosts)
    # No Relationship
    
    cor.test(jh_with_score$jh_sentiment_tweet, jh_with_score$Klout.Score)
    # No Relationship
      
    
    # Gives the average sentiment score across words
    jh_sent <- jackhoney_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt)

    jh_avg_sent_word <- jackhoney_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      summarise(sentiment = mean(polarity))
    # Jack Honey average sentiment by word = 0.12

    # Gives the average sentiment score across tweets
    jh_avg_sent_tweet <- jh_with_score %>%
      summarise(sentiment = mean(jh_sentiment_tweet))
    # Jack Honey average sentiment by tweet = 0.17

    t.test(jh_sent$polarity) 
    # Postive Jack Honey sentiment by word, p < .001
    t.test(jh_with_score$jh_sentiment_tweet) 
    # Positive Jack Honey sentiment by tweet, p < .001

    # Average sentiment by Category for Jack Honey Tweets
    jh_sent_cat <- jh_with_score %>%
      group_by(Category) %>% 
      summarise(sentiment_category = mean(jh_sentiment_tweet))
    # Note:  Positve sentiment category gives sentiment of 0.19
    
    # Most common positive, neutral, and negative words
    ##########################################################################
    
    jh_words_sentiment <- jackhoney_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      count(word, polarity, sort = TRUE) %>%
      ungroup()
    
    jh_words_sentiment$polarity <- factor(jh_words_sentiment$polarity,
                                          levels = c("-1", "0", "1"),
                                          labels = c("negative", "neutral", "postive"))
    
    jh_words_sentiment$polarity <- as.character(jh_words_sentiment$polarity)
    
    jh_words_sentiment
    
    jh_words_sentiment %>%
      group_by(polarity) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = polarity)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~polarity, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      ggtitle("Most Common Sentiment Words for Jack Honey") +
      theme(plot.title = element_text(hjust = .5)) +
      coord_flip()
    
##############################################################################
# Jager Sentiment Analysis
##############################################################################
    # Giving a sentiment score for each Jager Tweet
    ## Gives polarity of positive, negative, or neutral for the tweet as a whole
    jag_sentiment_score <- jager_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      group_by(line) %>%
      summarise(jag_sentiment_tweet = sum(polarity))
    
    
    # Gives sentiment score as a continuous variable
    jag_with_score <- merge(jag_sentiment_score, 
                                  contents_jager, by = "line")
    
    # Gives the average sentiment score across words
    jag_sent <- jager_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt)
    
    jag_avg_sent_word <- jager_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      summarise(sentiment = mean(polarity))
    # Jager average sentiment by word = 0.067
    
    # Gives the average sentiment score across tweets
    jag_avg_sent_tweet <- jag_with_score %>%
      summarise(sentiment = mean(jag_sentiment_tweet))
    # Jager average sentiment by tweet = 0.11
    
    t.test(jag_sent$polarity) 
    # Postive Jager sentiment by word, p < 0.001
    t.test(jag_with_score$jag_sentiment_tweet) 
    # Positive Jager sentiment by tweet, p < 0.001
    
    # Average sentiment by Category for Jager Tweets
    jag_sent_cat <- jag_with_score %>%
      group_by(Category) %>% 
      summarise(sentiment_category = mean(jag_sentiment_tweet))
    # Note:  Positve sentiment category gives sentiment of 0.066
    
    # Most common positive, neutral, and negative words
    ##########################################################################
    
    jag_words_sentiment <- jager_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      count(word, polarity, sort = TRUE) %>%
      ungroup()
    
    jag_words_sentiment$polarity <- factor(jag_words_sentiment$polarity,
                                          levels = c("-1", "0", "1"),
                                          labels = c("negative", "neutral", "postive"))
    
    jag_words_sentiment$polarity <- as.character(jag_words_sentiment$polarity)
    
    jag_words_sentiment
    
    jag_words_sentiment %>%
      group_by(polarity) %>%
      top_n(5) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = polarity)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~polarity, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      ggtitle("Most Common Sentiment Words for Jager") +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5)) +
      theme(legend.position = "none",
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Format X and Y Labels and Values
            axis.text.x = element_text(face = "bold.italic", color = "black", size = 13),
            axis.text = element_text(face = "bold", color = "black", size = 13),
            title = element_text(face = "bold", color = "black", size = 16),
            axis.title = element_text(face = "bold.italic", color = "black", size = 16)) +
      coord_flip() +
      scale_colour_brewer(palette = "OrRd")
      
    
##############################################################################
# Fireball Sentiment Analysis
##############################################################################
    # Giving a sentiment score for each Jager Tweet
    ## Gives polarity of positive, negative, or neutral for the tweet as a whole
    fb_sent_score <- fireball_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      group_by(line) %>%
      summarise(fb_sentiment_tweet = sum(polarity))
    
    
    # Gives sentiment score as a continuous variable
    fb_with_score <- merge(fb_sent_score, 
                                  contents_fireball, by = "line")
    
    # Gives the average sentiment score across words
    fb_sent <- fireball_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt)
    
    fb_avg_sent_word <- fireball_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      summarise(sentiment = mean(polarity))
    # Fireball average sentiment by word = 0.006
    
    # Gives the average sentiment score across tweets
    fb_avg_sent_tweet <- fb_with_score %>%
      summarise(sentiment = mean(fb_sentiment_tweet))
    # Fireball average sentiment by tweet = 0.0099
    
    t.test(fb_sent$polarity) 
    # Neutral Fireball sentiment by word, accept null
    t.test(fb_with_score$fb_sentiment_tweet) 
    # Neutral Fireball sentiment by tweet, accept null
    
    # Average sentiment by Category for Fireabll Tweets
    fb_sent_cat <- fb_with_score %>%
      group_by(Category) %>% 
      summarise(sentiment_category = mean(fb_sentiment_tweet))
    # Note:  Positve sentiment category gives sentiment of 0.36   
    
    
    # Most common positive, neutral, and negative words
    ##########################################################################
    
    fb_words_sentiment <- fireball_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      count(word, polarity, sort = TRUE) %>%
      ungroup()
    
    fb_words_sentiment$polarity <- factor(fb_words_sentiment$polarity,
                                                levels = c("-1", "0", "1"),
                                                labels = c("negative", "neutral", "postive"))
    
    fb_words_sentiment$polarity <- as.character(fb_words_sentiment$polarity)
    
    fb_words_sentiment
    
    fb_words_sentiment$word <- gsub("soltar", "drop", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("voltar", "come_back", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("solta", "drop_", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("torcida", "crowd", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("tocar", "touch", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("passos", "steps", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("tomar", "to_take", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("cara", "face", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("mano", "bro", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("comprar", "purchase", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("passar", "pass", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("beber", "drink", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("jogar", "play", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("ficar", "stay", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("reais", "real", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("vir", "come_over", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("ruim", "bad", fb_words_sentiment$word)
    fb_words_sentiment$word <- gsub("amigos", "friends", fb_words_sentiment$word)
    
    
    
    
    
    
    
    
    
    fb_words_sentiment %>%
      group_by(polarity) %>%
      top_n(5) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = polarity)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~polarity, scales = "free_y") +
      labs(y = "Contribution to Sentiment",
           x = NULL) +
      ggtitle("Top 5 Sentiment Words for Fireball") +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5)) +
      theme(legend.position = "none",
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Format X and Y Labels and Values
            axis.text.x = element_text(face = "bold.italic", color = "black", size = 12),
            axis.text = element_text(face = "bold", color = "black", size = 13),
            title = element_text(face = "bold", color = "black", size = 16),
            axis.title = element_text(face = "bold.italic", color = "black", size = 16)) +
      theme(plot.margin=unit(c(1,1,1,1),"cm")) +
      coord_flip() +
      scale_colour_brewer(palette = "OrRd") +
      ylim(0,25)
      
    # Fireball sentiment over time
    
    fb_time_sentiment <- fireball_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      arrange(Date)
    
    fb_time_sentiment$polarity <- factor(fb_time_sentiment$polarity,
                                               levels = c("-1", "0", "1"),
                                               labels = c("negative", "neutral", "positive"))
    
    fb_time_sentiment$polarity <- as.character(fb_time_sentiment$polarity)  
    
    fb_time_sentiment$Quarter <- NA
    
    for (i in 1:nrow(fb_time_sentiment)) {
      
      if(fb_time_sentiment[i, 3] >= "2017-01-01" & fb_time_sentiment[i,3] <= "2017-03-31") {
        fb_time_sentiment$Quarter[i] <- "Q1"
      } else if(fb_time_sentiment[i, 3] >= "2017-04-01" & fb_time_sentiment[i,3] <= "2017-06-30") {
        fb_time_sentiment$Quarter[i] <- "Q2"
      } else if(fb_time_sentiment[i, 3] >= "2017-07-01" & fb_time_sentiment[i,3] <= "2017-09-30") {
        fb_time_sentiment$Quarter[i] <- "Q3"
      } else if(fb_time_sentiment[i, 3] >= "2017-10-01" & fb_time_sentiment[i,3] <= "2017-11-13") {
        fb_time_sentiment$Quarter[i] <- "Q4"
      }
    }
    
    fb_time_sentiment_plot <- fb_time_sentiment %>%
      dplyr::count(Date, polarity) %>%
      spread(polarity, n, fill = 0) %>%
      mutate(polarity = positive - negative) %>%
      ggplot(aes(Date, polarity)) +
      geom_col(show.legend = FALSE) +
      ggtitle("Fireball Sentiment Over Time") +
      ylab("Sentiment")
    
    fb_time_sentiment_plot
    
    fb_time_sentiment_plot <- fb_time_sentiment %>%
      dplyr::count(Quarter, polarity) %>%
      spread(polarity, n, fill = 0) %>%
      mutate(polarity = positive - negative) %>%
      ggplot(aes(Quarter, polarity)) +
      geom_col(show.legend = FALSE) +
      ggtitle("Fireball Sentiment Over Time") +
      ylab("Sentiment")
    
    fb_time_sentiment_plot
    
    fb_time_sentiment$Month <- NA
    
    for (i in 1:nrow(fb_time_sentiment)) {
      
      if(fb_time_sentiment[i, 3] >= "2017-01-01" & fb_time_sentiment[i,3] <= "2017-01-31") {
        fb_time_sentiment$Month[i] <- "01"
      } else if(fb_time_sentiment[i, 3] >= "2017-02-01" & fb_time_sentiment[i,3] <= "2017-02-28") {
        fb_time_sentiment$Month[i] <- "02"
      } else if(fb_time_sentiment[i, 3] >= "2017-03-01" & fb_time_sentiment[i,3] <= "2017-03-31") {
        fb_time_sentiment$Month[i] <- "03"
      } else if(fb_time_sentiment[i, 3] >= "2017-04-01" & fb_time_sentiment[i,3] <= "2017-04-30") {
        fb_time_sentiment$Month[i] <- "04"
      }else if(fb_time_sentiment[i, 3] >= "2017-05-01" & fb_time_sentiment[i,3] <= "2017-05-31") {
        fb_time_sentiment$Month[i] <- "05"
      } else if(fb_time_sentiment[i, 3] >= "2017-06-01" & fb_time_sentiment[i,3] <= "2017-06-30") {
        fb_time_sentiment$Month[i] <- "06"
      } else if(fb_time_sentiment[i, 3] >= "2017-07-01" & fb_time_sentiment[i,3] <= "2017-07-31") {
        fb_time_sentiment$Month[i] <- "07"
      } else if(fb_time_sentiment[i, 3] >= "2017-08-01" & fb_time_sentiment[i,3] <= "2017-08-31") {
        fb_time_sentiment$Month[i] <- "08"
      } else   if(fb_time_sentiment[i, 3] >= "2017-09-01" & fb_time_sentiment[i,3] <= "2017-09-30") {
        fb_time_sentiment$Month[i] <- "09"
      } else if(fb_time_sentiment[i, 3] >= "2017-10-01" & fb_time_sentiment[i,3] <= "2017-10-31") {
        fb_time_sentiment$Month[i] <- "10"
      } else if(fb_time_sentiment[i, 3] >= "2017-11-01" & fb_time_sentiment[i,3] <= "2017-11-30") {
        fb_time_sentiment$Month[i] <- "11"
      } else if(fb_time_sentiment[i, 3] >= "2017-12-01" & fb_time_sentiment[i,3] <= "2017-12-31") {
        fb_time_sentiment$Month[i] <- "12"
      }
      
    }
    
    fb_time_sentiment <- plyr::arrange(fb_time_sentiment, Month)
    
    fb_time_sentiment_plot <- fb_time_sentiment %>%
      dplyr::count(Month, polarity) %>%
      spread(polarity, n, fill = 0) %>%
      mutate(polarity = positive - negative) %>%
      ggplot(aes(Month, polarity)) +
      geom_col(show.legend = FALSE) +
      ggtitle("Fireball Sentiment Over Time") +
      ylab("Sentiment")
    
    fb_time_sentiment_plot
    
    
##############################################################################
# Jack Fire Sentiment Analysis
##############################################################################
    # Giving a sentiment score for each Jager Tweet
    ## Gives polarity of positive, negative, or neutral for the tweet as a whole
    jf_sent_score <- jackfire_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      group_by(line) %>%
      summarise(jf_sentiment_tweet = sum(polarity))
    
    
    # Gives sentiment score as a continuous variable
    jf_with_score <- merge(jf_sent_score, 
                           contents_jackfire, by = "line")
    
    # Gives the average sentiment score across words
    jf_sent <- jackfire_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt)
    
    jf_avg_sent_word <- jackfire_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      summarise(sentiment = mean(polarity))
    # Jack Fire average sentiment by word = 0.14
    
    # Gives the average sentiment score across tweets
    jf_avg_sent_tweet <- jf_with_score %>%
      summarise(sentiment = mean(jf_sentiment_tweet))
    # Jack Fire average sentiment by tweet = 0.23
    
    t.test(jf_sent$polarity) 
    # Positive Jack Fire Sentiment by word, p = 0.0013
    t.test(jf_with_score$jf_sentiment_tweet) 
    # Positive Jack Fire Sentiment by tweet, p = 0.0027
    
    # Average sentiment by Category for Fireabll Tweets
    jf_sent_cat <- jf_with_score %>%
      group_by(Category) %>% 
      summarise(sentiment_category = mean(jf_sentiment_tweet))
    # Note:  Positve sentiment category gives sentiment of 0.27 
    
    # Most common positive, neutral, and negative words
    jackfire_word_sentiment <- jackfire_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      count(word, polarity, sort = TRUE) %>%
      ungroup()
    
    jackfire_word_sentiment$polarity <- factor(jackfire_word_sentiment$polarity,
                                               levels = c("-1", "0", "1"),
                                               labels = c("negative", "neutral", "positive"))
    
    jackfire_word_sentiment$polarity <- as.character(jackfire_word_sentiment$polarity)
    
    jackfire_word_sentiment
    
    jackfire_word_sentiment$word <- gsub("bebidas","drinks",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("louco","crazy",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("bebida","beverage",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("triste","sad",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("sorteadas","drawn",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("sair","get_out",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("role","role",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("redonda","gift",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("maluco","crazy_",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("fechar","close",jackfire_word_sentiment$word) 
    jackfire_word_sentiment$word <- gsub("bebada","drunk",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("tomar","take",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("beber","drink",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("comprar","purchase",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("falar","speak",jackfire_word_sentiment$word)      
    jackfire_word_sentiment$word <- gsub("deixas","I_gave",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("cara","face",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("provar","prove",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("igual","equal",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("ganhar","win",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("apaixonada","in_love",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("experimentar","experiment",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("melhor","best",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("mundo","world",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("gostoso","tasty",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("vi","saw",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("tranquilo","relax",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("reais","real",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("preciso","need",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("maravilhosa","wonderfil",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("ficar","stay",jackfire_word_sentiment$word)
    jackfire_word_sentiment$word <- gsub("mano","bro",jackfire_word_sentiment$word)
    
    jackfire_word_sentiment
    
    jackfire_word_sentiment %>%
      group_by(polarity) %>%
      top_n(5) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = polarity)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~polarity, scales = "free_y") +
      labs(y = "Contribution to Sentiment",
           x = NULL) +
      ggtitle("Top 5 Sentiment Words for Jack Fire") +
      theme_bw() +
      theme(plot.title = element_text(hjust = .5)) +
      theme(legend.position = "none",
            panel.border = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            # Format X and Y Labels and Values
            axis.text.x = element_text(face = "bold.italic", color = "black", size = 12),
            axis.text = element_text(face = "bold", color = "black", size = 13),
            title = element_text(face = "bold", color = "black", size = 16),
            axis.title = element_text(face = "bold.italic", color = "black", size = 16)) +
      theme(plot.margin=unit(c(1,1.5,1,1),"cm")) +
      coord_flip() +
      scale_colour_brewer(palette = "OrRd")
      
    
    # Jack Fire sentiment over time
    
    jackfire_time_sentiment <- jackfire_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      inner_join(lexicon_pt) %>%
      arrange(Date)
    
    jackfire_time_sentiment$polarity <- factor(jackfire_time_sentiment$polarity,
                                               levels = c("-1", "0", "1"),
                                               labels = c("negative", "neutral", "positive"))
    
    jackfire_time_sentiment$polarity <- as.character(jackfire_time_sentiment$polarity)  
    
    jackfire_time_sentiment$Quarter <- NA
    
    for (i in 1:nrow(jackfire_time_sentiment)) {
      
      if(jackfire_time_sentiment[i, 3] >= "2017-01-01" & jackfire_time_sentiment[i,3] <= "2017-03-31") {
        jackfire_time_sentiment$Quarter[i] <- "Q1"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-04-01" & jackfire_time_sentiment[i,3] <= "2017-06-30") {
        jackfire_time_sentiment$Quarter[i] <- "Q2"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-07-01" & jackfire_time_sentiment[i,3] <= "2017-09-30") {
        jackfire_time_sentiment$Quarter[i] <- "Q3"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-10-01" & jackfire_time_sentiment[i,3] <= "2017-11-13") {
        jackfire_time_sentiment$Quarter[i] <- "Q4"
      }
    }
    
    jackfire_time_sentiment_plot <- jackfire_time_sentiment %>%
      dplyr::count(Date, polarity) %>%
      spread(polarity, n, fill = 0) %>%
      mutate(polarity = positive - negative) %>%
      ggplot(aes(Date, polarity)) +
      geom_col(show.legend = FALSE) +
      ggtitle("Jack Fire Sentiment Over Time") +
      ylab("Sentiment")
    
    jackfire_time_sentiment_plot
    
    jackfire_time_sentiment_plot <- jackfire_time_sentiment %>%
      dplyr::count(Quarter, polarity) %>%
      spread(polarity, n, fill = 0) %>%
      mutate(polarity = positive - negative) %>%
      ggplot(aes(Quarter, polarity)) +
      geom_col(show.legend = FALSE) +
      ggtitle("Jack Fire Sentiment Over Time") +
      ylab("Sentiment")
    
    jackfire_time_sentiment_plot
    
    jackfire_time_sentiment$Month <- NA
    
    for (i in 1:nrow(jackfire_time_sentiment)) {
      
      if(jackfire_time_sentiment[i, 3] >= "2017-01-01" & jackfire_time_sentiment[i,3] <= "2017-01-31") {
        jackfire_time_sentiment$Month[i] <- "01"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-02-01" & jackfire_time_sentiment[i,3] <= "2017-02-28") {
        jackfire_time_sentiment$Month[i] <- "02"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-03-01" & jackfire_time_sentiment[i,3] <= "2017-03-31") {
        jackfire_time_sentiment$Month[i] <- "03"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-04-01" & jackfire_time_sentiment[i,3] <= "2017-04-30") {
        jackfire_time_sentiment$Month[i] <- "04"
      }else if(jackfire_time_sentiment[i, 3] >= "2017-05-01" & jackfire_time_sentiment[i,3] <= "2017-05-31") {
        jackfire_time_sentiment$Month[i] <- "05"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-06-01" & jackfire_time_sentiment[i,3] <= "2017-06-30") {
        jackfire_time_sentiment$Month[i] <- "06"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-07-01" & jackfire_time_sentiment[i,3] <= "2017-07-31") {
        jackfire_time_sentiment$Month[i] <- "07"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-08-01" & jackfire_time_sentiment[i,3] <= "2017-08-31") {
        jackfire_time_sentiment$Month[i] <- "08"
      } else   if(jackfire_time_sentiment[i, 3] >= "2017-09-01" & jackfire_time_sentiment[i,3] <= "2017-09-30") {
        jackfire_time_sentiment$Month[i] <- "09"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-10-01" & jackfire_time_sentiment[i,3] <= "2017-10-31") {
        jackfire_time_sentiment$Month[i] <- "10"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-11-01" & jackfire_time_sentiment[i,3] <= "2017-11-30") {
        jackfire_time_sentiment$Month[i] <- "11"
      } else if(jackfire_time_sentiment[i, 3] >= "2017-12-01" & jackfire_time_sentiment[i,3] <= "2017-12-31") {
        jackfire_time_sentiment$Month[i] <- "12"
      }
      
    }
    
    jackfire_time_sentiment <- plyr::arrange(jackfire_time_sentiment, Month)
    
    jackfire_time_sentiment_plot <- jackfire_time_sentiment %>%
      dplyr::count(Month, polarity) %>%
      spread(polarity, n, fill = 0) %>%
      mutate(polarity = positive - negative) %>%
      ggplot(aes(Month, polarity)) +
      geom_col(show.legend = FALSE) +
      ggtitle("Jack Fire Sentiment Over Time") +
      ylab("Sentiment")
    
    jackfire_time_sentiment_plot
    
   
######## T tests for differences in sentiment between brands using individual words
    
  
    t.test(jf_sent$polarity, fb_sent$polarity)
    # Jack Fire Sentimnet > Fireball Sentiment, p = 0.019
    
    t.test(jf_sent$polarity, jag_sent$polarity)
    # No Difference in Jack Fire and Jager Sentiment, p = 0.13
    
    t.test(jf_sent$polarity, jh_sent$polarity)
    # No difference in Jack Fire and Jack Honey Sentiment, p = 0.64
    
    t.test(jh_sent$polarity, fb_sent$polarity)
    # Jack Honey Sentiment > Fireball Sentiment, p = 0.006
    
    t.test(jh_sent$polarity, jag_sent$polarity)
    # Jack Honey Sentiment > Jager Sentiment, p = 0.04

    
######## T tests for differences in sentiment between brands using indiviual tweets

    t.test(jf_with_score$jf_sentiment_tweet, fb_with_score$fb_sentiment_tweet)    
    # Jack Fire Sentimnet > Fireball Sentiment, p = 0.024
    
    t.test(jf_with_score$jf_sentiment_tweet, jag_with_score$jag_sentiment_tweet)
    # No Difference in Jack Fire and Jager Sentiment, p = 0.13
    
    t.test(jf_with_score$jf_sentiment_tweet, jh_with_score$jh_sentiment_tweet)
    # No difference in Jack Fire and Jack Honey Sentiment, p = 0.49
    
    t.test(jh_with_score$jh_sentiment_tweet, fb_with_score$fb_sentiment_tweet)
    # Jack Honey Sentiment > Fireball Sentiment, p = 0.013
    
    t.test(jh_with_score$jh_sentiment_tweet, jag_with_score$jag_sentiment_tweet)
    # Jack Honey Sentiment > Jager Sentiment, p = 0.076
    
##############################################################################
##############################################################################
# Analyzing word and document frequency: tf-idf
##############################################################################
##############################################################################
    
    ##### Term Frequency
    
    # Jack Honey, Jack Fire, Jager, Fireball
    # Take each of the "..._word" dataframes from above
    # Count each word, also removing stopwords
    # Make sure to create a column representing the 4 prouducts
    jh_word_count <- jackhoney_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      count(word, sort = TRUE)
    
    jf_word_count <- jackfire_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      count(word, sort = TRUE)
    
    jag_word_count <- jager_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      count(word, sort = TRUE)
    
    fb_word_count <- fireball_word %>%
      anti_join(stop_pt) %>%
      anti_join(stop_en) %>%
      count(word, sort = TRUE)
    
    tweet_words <- bind_rows(mutate(jh_word_count, product = "Jack Honey"),
                          mutate(jf_word_count, product = "Jack Fire"),
                          mutate(jag_word_count, product = "Jager"),
                          mutate(fb_word_count, product = "Fireball"))
    
    total_words <- tweet_words %>%
      group_by(product) %>%
      summarise(total = sum(n))
    
    tweet_words <- left_join(tweet_words, total_words)
    
    ggplot(tweet_words, aes(n/total, fill = product)) +
      geom_histogram(show.legend = FALSE) +
      xlim(NA, 0.008) +
      facet_wrap(~product, ncol = 2, scales = "free_y")
    
    ##### Zipf's Law
    
    freq_by_rank <- tweet_words %>%
      group_by(product) %>%
      mutate(rank = row_number(),
             `term frequency` = n/total)
    
    lm(log10(`term frequency`) ~ log10(rank), data = freq_by_rank)
    
    freq_by_rank %>%
      ggplot(aes(rank, `term frequency`, color = product)) +
      geom_abline(intercept = -1.4780, slope = -.7624,
                  color = "gray50",
                  linetype = 2) +
      geom_line(size = 1.2, alpha = 0.8) +
      scale_x_log10() +
      scale_y_log10()
    
    
    
    ##### The 'bind_tf_idf' function
    
    tweet_words <- tweet_words %>%
      bind_tf_idf(word, product, n)
    
    tweet_words
    
    # Arrange by desceding tf_idf to look at terms with high tf-idf
    tweet_words %>%
      select(-total) %>%
      arrange(desc(tf_idf))
    
    # Construct a visualization of the high tf-idf words across all products
    plot_tweets <- tweet_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word = factor(word, levels = rev(unique(word))))
    
    plot_tweets %>%
      top_n(20) %>%
      ggplot(aes(word, tf_idf, fill = product)) +
      geom_col() +
      labs( x = NULL, y = "tf_idf") +
      coord_flip()
    
    # Looking at the products indidually
    plot_tweets %>%
      group_by(product) %>%
      top_n(20) %>%
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = product)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~product, ncol = 2, scales = "free") +
      coord_flip() +
      ggtitle("tf-idf by Product") +
      theme(plot.title = element_text(hjust = .5))
    
##############################################################################
# Relationship between words: n-grams and correlations
##############################################################################
    
    ##### Analyzing bigrams
    
    jackfire_bigrams %>%
      filter(word2 == "red") %>%
      count(word1, sort = TRUE)
      
##############################################################################
    # Misc Code Testing
    
    sentiment_words <- bind_rows(mutate(jh_words_sentiment, product = "Jack Honey"),
                             mutate(jackfire_word_sentiment, product = "Jack Fire"),
                             mutate(jag_words_sentiment, product = "Jager"),
                             mutate(fb_words_sentiment, product = "Fireball"))
     

    
    
    jackfire_word <- contents_jackfire %>%
      unnest_tokens(word, text)  

    test <- select(jackfire_word, word)    

    
    install.packages("quanteda")
    library(quanteda)
    
    tokens <- tokens(test, method = "chi2", max_size = 2)   

    
    test <- test$word    
    
    
    corp <- Corpus(VectorSource(jackfire$Contents))
    corp <- tm_map(corp, removeWords, stop_pt$word)
    corp <- tm_map(corp, removeWords, stop_en$word)
    
    corp <- corpus(corp)
    
    coll <- textstat_collocations(corp, method = "lambda")
        