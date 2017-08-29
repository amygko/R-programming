#In order to get harrypotter data, you need these packages
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}

library(memoise)
library(withr)
library(curl)
library(httr)

devtools::install_github("bradleyboehmke/harrypotter")
library(harrypotter)

library(tidyverse)      # data manipulation & plotting
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(harrypotter)    # provides the first seven novels of the Harry Potter series

#harry potter package include the seven books of harry potter
#For example,

philosophers_stone[1:2]
sentiments #tidytext package has three sentiment lexicons in sentiments data
 # Three lexicons are AFINN, bing, nrc
#The bing lexicon categorizes words in a binary fashion into positive and negative categories. 
#The AFINN lexicon assigns words with a score that runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment. 
#The nrc lexicon categorizes words in a binary fashion (“yes”/“no”) into categories of positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust. 

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#basic sentiment analysis

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series<-tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))

series


#Now lets use the nrc sentiment data set to assess the different sentiments that are represented across the Harry Potter series. 
#We can see that there is a stronger negative presence than positive

series %>%
  right_join(get_sentiments("nrc")) %>%
  filter(!is.na(sentiment)) %>%
  count(sentiment, sort = TRUE)

series %>%
  group_by(book) %>% 
  mutate(word_count = 1:n(),
         index = word_count %/% 500 + 1) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(book, index = index , sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book, levels = titles)) %>%
  ggplot(aes(index, sentiment, fill = book)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free_x")

#1.create an index that breaks up each book by 500 words; this is the approximate number of words on every two pages so this will allow us to assess changes in sentiment even within chapters
#2.join the bing lexicon with inner_join to assess the positive vs. negative sentiment of each word
#3.count up how many positive and negative words there are for every two pages”
#4.spread our data and…
#5.calculate a net sentiment (positive - negative)
#6.plot our data







#word cloud
library(NLP)
library(tm) # for text mining
library(SnowballC)  # for text stemming
library(RColorBrewer)  # color palettes
library(wordcloud)  # word-cloud generator 

#First, we need to create a corpus.
harryCorpus <- Corpus(VectorSource(philosopher_stone))

#Next, we will convert the corpus to a plain text document.

harryCorpus <- tm_map(harryCorpus, PlainTextDocument)

#Then, we will remove all punctuation and stopwords. Stopwords are commonly used words in the English language such as I, me, my, etc. 
#You can see the full list of stopwords using stopwords('english').

harryCorpus <- tm_map(harryCorpus, removePunctuation)
harryCorpus <- tm_map(harryCorpus, removeNumbers)
harryCorpus <- tm_map(harryCorpus, removeWords, stopwords('english'))

#After removing redundant words, let's create the corpus with the new one.
harryCorpus<-Corpus(VectorSource(harryCorpus))



#Next, we will perform stemming. This means that all the words are converted to their stem (Ex: learning -> learn, walked -> walk, etc.). 
#This will ensure that different forms of the word are converted to the same form and plotted only once in the wordcloud.

harryCorpus <- tm_map(harryCorpus, stemDocument)

#
wordcloud(harryCorpus, max.words = 200, random.order = FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"))

harryCorpus <- Corpus(VectorSource(philosophers_stone))


