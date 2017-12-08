install.packages('tidytext')
install.packages('lexiconPT')
install.packages("stopwords")
library(tidyverse)
library(tidytext)
library(lexiconPT)
library(ggplot2)
library(stopwords)
library(tidyr)
library(readxl)

# Read in the twitter data
data <- read_xlsx("data.xlsx")

glimpse(data)

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

# Exlporing Gender
data$Gender <- as.factor(data$Gender)


contents <- data %>%
  mutate(line = row_number(), text = Contents)

contents

contents_word <- contents %>%
  unnest_tokens(word, text)

lexicon_pt <- oplexicon_v2.1 %>%
  mutate(word = term)


# Read in the portuguese stopwords
stop_pt <- data_frame(word = stopwords$pt)

# Average sentiment by Gender
sentiment <- contents_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  group_by(Gender) %>%
  summarise(sentiment = mean(polarity))


# Try using the stopwords package as well

n_plot <- contents_word %>%
  dplyr::anti_join(stop_pt) %>%
  dplyr::count(word) %>%
  dplyr::filter(n > 1000) %>%
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
  filter(!word2 %in% stop_pt$word)

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
  filter(n > 300) %>%
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
         !word3 %in% stop_pt$word)


trigrams_united <- contents_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

n_plot_trigrams <- trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  filter(n > 50) %>%
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

fireball <- read_xlsx("fireball.xlsx")
jackfire <- read_xlsx("jackfire.xlsx")
jackhoney <- read_xlsx("jackhoney.xlsx")
jager <- read_xlsx("jager.xlsx")

##############################################################################
# jack honey

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

# Create a dataframe of the tweets only, which has a column name of 'Contents'
contents_jackhoney <- jackhoney %>%
  mutate(line = row_number(), text = Contents) %>%
  arrange(Category, Date)

contents_jackhoney

jackhoney_word <- contents_jackhoney %>%
  unnest_tokens(word, text)

jackhoney_sentiment <- jackhoney_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  group_by(Category) %>% # take away line to group by category
  summarise(sentiment = mean(polarity))


## Gives polarity of positive, negative, or neutral for the tweet as a whole

jackhoney_sent <- jackhoney_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt)

jackhoney_average_sent <- jackhoney_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  summarise(sentiment = mean(polarity))


t.test(jackhoney_sent$polarity)

# Try using the stopwords package as well

jackhoney_n_plot <- jackhoney_word %>%
  anti_join(stop_pt) %>%
  count(word, sort=TRUE) %>%
  filter(n > 100) %>%
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
         !word2 %in% stop_pt$word)

jackhoney_bigrams_united <- jackhoney_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

jackhoney_n_plot_bigrams <- jackhoney_bigrams_united %>%
  # anti_join(stop_pt) %>%
  count(bigram, sort=TRUE) %>%
  filter(n > 15) %>%
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
         !word3 %in% stop_pt$word)


jackhoney_trigrams_united <- jackhoney_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

jackhoney_n_plot_trigrams <- jackhoney_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  filter(n > 3) %>%
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
jager
########################################################################################

# Create a dataframe of the tweets only, which has a column name of 'Contents'
contents_jager <- jager %>%
  mutate(line = row_number(), text = Contents) %>%
  arrange(Category, Date)

jager_word <- contents_jager %>%
  unnest_tokens(word, text)

jager_sentiment <- jager_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  group_by(Category) %>% # take away line to group by category
  summarise(sentiment = mean(polarity))


## Gives polarity of positive, negative, or neutral for the tweet as a whole

jager_sent <- jager_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt)

jager_average_sent <- jager_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  summarise(sentiment = mean(polarity))

# Try using the stopwords package as well

jager_n_plot <- jager_word %>%
  anti_join(stop_pt) %>%
  dplyr::count(word, sort=TRUE) %>%
  filter(n > 100) %>%
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
         !word2 %in% stop_pt$word)

jager_bigrams_united <- jager_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

jager_n_plot_bigrams <- jager_bigrams_united %>%
  # anti_join(stop_pt) %>%
  dplyr::count(bigram, sort=TRUE) %>%
  filter(n > 15) %>%
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
         !word3 %in% stop_pt$word)


jager_trigrams_united <- jager_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

jager_n_plot_trigrams <- jager_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  filter(n > 6) %>%
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
# fireball
##############################################################################

# Creating a new column called 'Date' that represents date format
fireball$Date <- as.Date(fireball$Date..EST., "%m/%d/%Y %H:%M")

# Arrange tweets in chronological order
fireball <- fireball %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(fireball$State.Region) # 28 unique regions
unique(fireball$City.Urban.Area) # 288 unique cities

# Exploring distribution of klout score
hist(fireball$Klout.Score)
summary(fireball$Klout.Score)

# Create a dataframe of the tweets only, which has a column name of 'Contents'
# Grouping by category here

contents_fireball <- fireball %>%
  mutate(line = row_number(), text = Contents) %>%
  arrange(Category, Date)

contents_fireball

fireball_word <- contents_fireball %>%
  unnest_tokens(word, text)


library(reshape2) 

##############################################################################
# Calculates a sentiment score for each tweet - if group_by 'line'
# Look at both average and sum
##############################################################################
fireball_sentiment <- fireball_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  group_by(Category) %>% # take away line to group by category
  summarise(sentiment = mean(polarity))

fireball_sent <- fireball_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt)

fireball_average_sent <- fireball_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  summarise(sentiment = mean(polarity))

t.test(fireball_sent$polarity)
t.test(fireball_sent$polarity, jackhoney_sent$polarity)

hist(fireball_sentiment$sentiment)
##############################################################################

# Try using the stopwords package as well

fireball_n_plot <- fireball_word %>%
  anti_join(stop_pt) %>%
  count(word, sort=TRUE) %>%
  filter(n > 25) %>%
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
         !word2 %in% stop_pt$word)

fireball_bigrams_united <- fireball_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

fireball_n_plot_bigrams <- fireball_bigrams_united %>%
  # anti_join(stop_pt) %>%
  count(bigram, sort=TRUE) %>%
  filter(n > 5) %>%
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
         !word3 %in% stop_pt$word)


fireball_trigrams_united <- fireball_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

fireball_n_plot_trigrams <- fireball_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  filter(n > 2 ) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  xlab("trigram") +
  ylab("word count") +
  ggtitle("Fireball Most Frequent Trigrams") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

fireball_n_plot_trigrams  # Notice the amount of english words!!!!

# Most common positive, neutral, and negative words

fireball_words_sentiment <- fireball_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  count(word, polarity, sort = TRUE) %>%
  ungroup()

fireball_words_sentiment$polarity <- factor(fireball_words_sentiment$polarity,
                                            levels = c("-1", "0", "1"),
                                            labels = c("negative", "neutral", "postive"))

fireball_words_sentiment$polarity <- as.character(fireball_words_sentiment$polarity)

fireball_words_sentiment

fireball_words_sentiment %>%
  group_by(polarity) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = polarity)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~polarity, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  ggtitle("Most Common Sentiment Words for Fireball") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()
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

##############################################################################
# Calculates a sentiment score for each tweet - if group_by 'line'
# Look at both average and sum
##############################################################################
jackfire_sentiment <- jackfire_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  group_by(Category) %>% # take away line to summarise by category
  summarise(sentiment = mean(polarity))


jackfire_sent <- jackfire_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt)

jackfire_average_sent <- jackfire_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  summarise(sentiment = mean(polarity))


t.test(jackfire_sent$polarity)
t.test(jackfire_sent$polarity, fireball_sent$polarity)


# Notice the positive sentiment for jackfire!!!!
# Sentiment for fireball was not so good!
# Statistically significant - need to test.


barplot(jackfire_sentiment$sentiment) # get a better visual!
##############################################################################

# Try using the stopwords package as well

jackfire_n_plot <- jackfire_word %>%
  anti_join(stop_pt) %>%
  count(word, sort=TRUE) %>%
  filter(n > 10) %>%
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
         !word2 %in% stop_pt$word)

jackfire_bigrams_united <- jackfire_bigrams %>%
  unite(bigram, word1, word2, sep=" ")

jackfire_n_plot_bigrams <- jackfire_bigrams_united %>%
  # anti_join(stop_pt) %>%
  count(bigram, sort=TRUE) %>%
  filter(n > 2) %>%
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

# Analyzing bigrams

library(igraph)
jackfire_bigrams_count <- jackfire_bigrams %>%
  count(word1, word2, sort = TRUE)

jackfire_bigram_graph <- jackfire_bigrams_count %>%
  filter(n > 2) %>%
  graph_from_data_frame()

install.packages("ggraph")
library(ggraph)
set.seed(2017)

ggraph(jackfire_bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


# jackfire trigrams


jackfire_trigrams <- contents_jackfire %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_pt$word,
         !word2 %in% stop_pt$word,
         !word3 %in% stop_pt$word)


jackfire_trigrams_united <- jackfire_trigrams %>%
  unite(trigram, word1, word2, word3, sep=" ")

jackfire_n_plot_trigrams <- jackfire_trigrams_united %>%
  # anti_join(stop_pt) %>%
  count(trigram, sort=TRUE) %>%
  filter(n > 1) %>%
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

# Most common positive, neutral, and negative words
jackfire_word_sentiment <- jackfire_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  count(word, polarity, sort = TRUE) %>%
  ungroup()

jackfire_word_sentiment$polarity <- factor(jackfire_word_sentiment$polarity,
                                           levels = c("-1", "0", "1"),
                                           labels = c("negative", "neutral", "positive"))

jackfire_word_sentiment$polarity <- as.character(jackfire_word_sentiment$polarity)

jackfire_word_sentiment

jackfire_word_sentiment %>%
  group_by(polarity) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = polarity)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~polarity, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  ggtitle("Most Common Sentiment Words for Jack Fire") +
  theme(plot.title = element_text(hjust = .5)) +
  coord_flip()

# Jack Fire sentiment over time

jackfire_time_sentiment <- jackfire_word %>%
  anti_join(stop_pt) %>%
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

##############################################################################

# Visual 





##############################################################################
install.packages("text2vec")
library(text2vec)
library(data.table)



######## T tests for differences in sentiment between brands

t.test(jager_sent$polarity)
t.test(jackfire_sent$polarity)
t.test(jackhoney_sent$polarity)
t.test(fireball_sent$polarity)
t.test(jackfire_sent$polarity, fireball_sent$polarity)
t.test(jackfire_sent$polarity, jager_sent$polarity)
t.test(jackfire_sent$polarity, jackhoney_sent$polarity)
t.test(jackhoney_sent$polarity, fireball_sent$polarity)
t.test(jackhoney_sent$polarity, jager_sent$polarity)
