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




# Arrange tweets in chronological order
data <- data %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(data$State) # 28 unique regions
unique(data$City) # 288 unique cities

# Exploring distribution of klout score


hist(data$Klout.Score)
summary(data$Klout.Score)

# Create a dataframe of the tweets only, which has a column name of 'Contents'
contents <- data %>%
  transmute(line = row_number(), text = Contents)

contents

contents_word <- contents %>%
  unnest_tokens(word, text)

lexicon_pt <- oplexicon_v2.1 %>%
  mutate(word = term)


# Read in the portuguese stopwords
stop_words <- read_csv("stopwords.csv")

stop_pt <- data_frame(word = stopwords$pt)

sentiment <- contents_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) 
  # count(word, sort = TRUE)

# Try using the stopwords package as well

n_plot <- contents_word %>%
  anti_join(stop_pt) %>%
  count(word, sort=TRUE) %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

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
  xlab(NULL) +
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
  coord_flip()

n_plot_trigrams

##############################################################################
# Tokenizing by N-gram - Filtered Data

# read in filtered data

fireball <- read_xlsx("fireball.xlsx")
jackfire <- read_xlsx("jackfire.xlsx")
jackhoney <- read_xlsx("jackhoney.xlsx")
jager <- read_xlsx("jager.xlsx")

##############################################################################
# honey jack



# Arrange tweets in chronological order
jackhoney <- jackhoney %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(cerveja$State.Region) # 28 unique regions
unique(cerveja$City.Urban.Area) # 288 unique cities

# Exploring distribution of klout score
hist(cerveja$Klout.Score)
summary(cerveja$Klout.Score)

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

# Try using the stopwords package as well

jackhoney_n_plot <- jackhoney_word %>%
  anti_join(stop_pt) %>%
  count(word, sort=TRUE) %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
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
  coord_flip()

jackhoney_n_plot_trigrams

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
  filter(n > 10) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
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
  filter(n > 5) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

fireball_n_plot_trigrams  # Notice the amount of english words!!!!


##############################################################################
# jackfire
##############################################################################

# Creating a new column called 'Date' that represents date format
jackfire$Date <- as.Date(jackfire$Date..EST., "%m/%d/%Y %H:%M")

# Arrange tweets in chronological order
jackfire <- jackfire %>%
  arrange(Date) %>%
  filter(Date >= '2017-01-01')

# Exploring the different regions and urban areas
unique(jackfire$State.Region) # 28 unique regions
unique(jackfire$City.Urban.Area) # 288 unique cities

# Exploring distribution of klout score
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


library(reshape2) 

##############################################################################
# Calculates a sentiment score for each tweet - if group_by 'line'
# Look at both average and sum
##############################################################################
jackfire_sentiment <- jackfire_word %>%
  anti_join(stop_pt) %>%
  inner_join(lexicon_pt) %>%
  group_by(Category) %>% # take away line to summarise by category
  summarise(sentiment = mean(polarity))

# Notice the positive sentiment for jackfire!!!!
# Sentiment for fireball was not so good!
# Statistically significant - need to test.


barplot(jackfire_sentiment$sentiment) # get a better visual!
##############################################################################

# Try using the stopwords package as well

jackfire_n_plot <- jackfire_word %>%
  anti_join(stop_pt) %>%
  count(word, sort=TRUE) %>%
  filter(n > 25) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
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
  filter(n > 1) %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

jackfire_n_plot_bigrams 

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
  coord_flip()

jackfire_n_plot_trigrams  

#  Notes - plot sentiment over time, use gender, klout score, region
##############################################################################
# jackhoney

