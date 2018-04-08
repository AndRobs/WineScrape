library(tidyverse)
library(tidytext)

Wine <-  read_csv("WineDf.csv")

# gives most common words
Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  ungroup() %>%
  count(word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_bar(aes(x = word, y = n), stat = 'identity', fill = '#A0C2CC') +
  geom_text(aes(x = word, y = n - 5, label = word), hjust = 1, size = 8) +
  coord_flip() +
  theme_minimal(20) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank()) +
  labs(y = 'Word occurrences')

# gives most common words with sentiments
Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  inner_join(get_sentiments("bing")) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  count(word) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  slice(1:10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot() +
  geom_bar(aes(x = word, y = n, fill = sentiment), stat = 'identity') +
  geom_text(aes(x = word, y = n - 5, label = word), hjust = 1, size = 8) +
  coord_flip() +
  theme_minimal(20) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'top') +
  labs(y = 'Word occurrences',
       fill = 'Sentiment:') +
  scale_fill_manual(values = c("#FFE3C6", "#A0C2CC" ))

# gives most negative words in data
Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  ungroup() %>%
  distinct(word) %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(score)

# gives most positive words in data
Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  ungroup() %>%
  distinct(word) %>%
  inner_join(get_sentiments("afinn")) %>%
  arrange(desc(score))
  

# gives overall description score
Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(names) %>%
  summarise(score = sum(score)) %>%
  ggplot() +
  geom_histogram(aes(x = score), binwidth = 1, fill = '#A0C2CC') +
  theme_minimal(20) +
  labs(x = 'Sentiment score', y = "Number of wines")

Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(names) %>%
  summarise(score = sum(score)) %>%
  summarise(avg = mean(score))

# returns most positive wine description 
Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(names) %>%
  summarise(score = sum(score)) %>%
  arrange(desc(score)) %>%
  slice(1) 

Wine %>% filter(names %in% c("Chteau Pigoudet La Chapelle Ros 2016 Coteaux dAix-en-Provence")) %>%
  select(descr) %>%
  write.csv

# returns most negative wine description ----------------------------------


Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(names) %>%
  summarise(score = sum(score)) %>%
  arrange(score) %>%
  slice(1) 

Wine %>% filter(names %in% c("Pedro Ximenez Triana Hidalgo")) %>%
  select(descr) %>%
  write.csv

Wine %>%
  group_by(names) %>% 
  unnest_tokens(word, descr) %>%
  anti_join(stop_words)  %>%
  inner_join(get_sentiments("afinn")) %>%
  filter(names %in% c("Pedro Ximenez Triana Hidalgo"))
