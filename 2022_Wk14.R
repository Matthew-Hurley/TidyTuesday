library(tidytuesdayR)
library(tidyverse)

# Read Data

tuesdata <- tidytuesdayR::tt_load(2022, week = 14)
news_orgs <- tuesdata$news_orgs

# Transform data
revenues <- na.omit(news_orgs$revenue_streams)
q <- ""
for(i in seq(1, length(revenues))){
  q <- paste(q, revenues[i], ',')}
revenue_sources <- q %>% str_replace_all("yet, but", "yet but") %>% str_split( ',')
revenue_sources <- as_tibble(revenue_sources, .name_repair = "universal")
revenue_sources$...1 <- str_trim(revenue_sources$...1)
revenue_sources <-revenue_sources%>% group_by(...1) %>% summarise(num = n())
revenue_sources <- revenue_sources[-c(1),]

# Plot data!
ggplot(revenue_sources, aes(x = reorder(...1, num), y = num))+
  geom_col(fill = "cornflowerblue") + coord_flip() +
  xlab("Source of revenue") + ylab("Count") +
  ggtitle("Sources of revenue for online publications in US and Canada")
