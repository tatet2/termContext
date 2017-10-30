library(lsa)
library(tidytext)
library(wordcloud)
library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)

get_sentiments("afinn")
get_sentiments("afinn")


r15 <- read.csv('/home/tate/dataIncApp/2017/projProp/redditRhino/redditAll.csv', stringsAsFactors=FALSE)

## tidy data and filter less relevant subreddits
redr <- r15 %>%
    mutate(datetime = as_datetime(created_utc, origin = "1970-01-01", tz = "UTC"), hour = hour(datetime), date = as_date(datetime)) %>%
    separate(date, c("year", "month", "day"), sep="-", remove=FALSE) %>% 
    select(body, subreddit, year, month, day, hour, date ) %>%
    filter(!(subreddit%in%c('trees','microgrowery', 'Marijuana')))

redr$comment <- rownames(redr)

redc <- redr %>%
    group_by(hour) %>%
    summarise(count = n())

ggplot(redc, aes(hour, count)) +
    geom_col(show.legend = FALSE)

red <- redr %>%
    unnest_tokens(word, body) 


reds <- red %>%
    inner_join(get_sentiments("bing")) %>%
    count(year, month, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)    

ggplot(reds, aes(month, sentiment, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 2, scales = "free_x")

## word counts
bing_word_counts <- red %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

bing_word_counts

custom_stop_words <- bind_rows(data_frame(word = c("wild", "weed", "fuck", "fucking", "shit"), 
                                          lexicon = c("custom")), 
                               stop_words)

bing_word_counts <- red %>%
    anti_join(custom_stop_words) %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()


bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


## word cloud
library(reshape2)

red %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    acast(word ~ sentiment, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                     max.words = 200)

## by comment
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- red %>%
  group_by(hour, comment) %>%
  summarize(words = n())

com <- red %>%
    semi_join(bingnegative) %>%
    group_by(hour, comment) %>%
    summarize(negativewords = n()) %>%
    left_join(wordcounts, by = c("hour", "comment")) %>%
    mutate(ratio = negativewords/words) %>%
    group_by(hour) %>%
    summarise(count = n(), meanRatio = mean(ratio))
    
ggplot(com, aes(hour, count, fill=positiveComment)) +
    geom_col(show.legend = FALSE)

ggplot(com, aes(hour, meanRatio)) +
    geom_line(show.legend = FALSE)


##
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tb %>%
  group_by(book, chapter) %>%
  summarize(words = n())

tb %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()


##
tb <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
    ungroup() %>%
    unnest_tokens(word, text)

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


library(reshape2)

 %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

##
ja <- austen_books()
jas <- janeaustensentiment <- tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(book, index = linenumber %/% 80, sentiment)
%>%
    spread(sentiment, n, fill = 0)

red %>%
    count(subreddit, sort=TRUE) 

##


    
    
    
