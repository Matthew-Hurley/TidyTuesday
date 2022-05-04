library(tidyverse)
library(RColorBrewer)
library(wordcloud)
library(wordcloud2)
library(tidytext)

# Load Data
tuesdata <- tidytuesdayR::tt_load(2022, week = 16)
times <- tuesdata$times

# Read answers column
text <- times$answer %>% str_to_lower()

# Obtain counts and make data frame
words <- as.data.frame(table(text))

# Take the top 200
words <- words %>% arrange(desc(Freq))
words <- words[1:200,]

# Create 'Word Cloud' using wordcloud
wordcloud(words = words$text, freq = words$Freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"), scale=c(1.8,0.08))

# Load Big Dave
big_dave <- tuesdata$big_dave

# Process in same way
text <- big_dave$answer %>% str_to_lower()
words <- as.data.frame(table(text))
words <- words %>% arrange(desc(Freq))
words <- words[1:200,]

# Create 'Word Cloud' using wordcloud2
wordcloud2(data=words, size=0.4, color='random-dark')
