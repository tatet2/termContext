library(lsa)
library(tidytext)
library(wordcloud)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(gridExtra)


r15 <- read.csv('redditAll.csv', stringsAsFactors=FALSE)

## tidy data and filter less relevant subreddits
redr <- r15 %>%
    mutate(datetime = as_datetime(created_utc, origin = "1970-01-01", tz = "UTC"), hour = hour(datetime), date = as_date(datetime)) %>%
    separate(date, c("year", "month", "day"), sep="-", remove=FALSE) %>% 
    select(body, subreddit, year, month, day, hour, date, score, ups, downs ) %>%
    filter(!(subreddit%in%c('trees','microgrowery', 'Marijuana')))
redr$comment <- rownames(redr)

redc <- redr %>%
    group_by(hour) %>%
    summarise(count = n())

red <- redr %>%
    unnest_tokens(word, body) 

reds <- red %>%
    inner_join(get_sentiments("bing")) %>%
    count(year, month, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)    

## plot sentiment by month, faceted by year
ggplot(reds, aes(month, sentiment, fill = year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~year, ncol = 2, scales = "free_x")

## word counts figure 1
bing_word_counts <- red %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

bing_word_counts
custom_stop_words <- bind_rows(data_frame(word = c("weed", "fuck", "fucking", "shit"), 
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
ggsave("wordCount.pdf")

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

## by score

afinn <- get_sentiments("afinn") 


wordcounts <- red %>%
  group_by(comment) %>%
  summarize(words = n(), score=mean(score))

center <- function(x){(x - mean(x))/sd(x)}
sc <- red %>%
    inner_join(afinn, by='word')%>%
    group_by(comment, hour) %>%
    summarise(wordscore = mean(score.y), rscore = mean(score.x)) %>%
    ungroup() 
sc$rscore_c <- center(sc$rscore)
sc$wscore_c <- center(sc$wordscore)


ggplot(data=sc, aes(y=log(rscore_c), x=wscore_c)) + geom_point() + stat_smooth()

ggplot(data=sc, aes(x=log(score), y=ratio)) + geom_point()
m <- with(sc, lm(ratio~log(score)))
expfun <- function(x){ exp(coef(m)[1] + coef(m)[2] * x) }
ggplot(data=sc, aes(x=log(score), y=ratio)) + geom_point() + stat_smooth()

## score by hour figure 2

p1 <- ggplot(redc, aes(hour, count)) +
    geom_col(show.legend = FALSE) + xlab("") + ylab("Comment Count")

sh <- sc %>%
    group_by(hour) %>%
    summarise(score=mean(wordscore))

p2 <- ggplot(sh, aes(x=hour, score)) +
    geom_line(show.legend = FALSE) + xlab("Hour of Day") + ylab("Sentiment")

ml <- arrangeGrob(p1,p2,nrow=2)    

pdf("CountOverSentHour.pdf")
plot(ml)
dev.off()
