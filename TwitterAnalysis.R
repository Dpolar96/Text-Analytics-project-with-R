# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
# plotting packages
library(igraph)
library(ggraph)
library(tidyverse)
library(httr)
library(tidytext)
library(tidyr)
library(stringr)

data("stop_words")

custom_stop_words <- tribble(
  ~word, ~lexicon,
  "http", "CUSTOM",
  "https", "CUSTOM",
  "destinythegame", "CUSTOM",
  "wow", "CUSTOM",
  "ffxiv", "CUSTOM",
  "warcraft", "CUSTOM",
  "bungie", "CUSTOM"
)

stop_words_rev <- stop_words %>% 
  bind_rows(custom_stop_words)

auth_setup_default()

destiny_df <- search_tweets(q = "DestinyTheGame", n = 5000,
                    lang = "en", since ='2018-01-01',
                    include_rts = FALSE)

destiny_text <- as.data.frame(destiny_df$text)
destiny_text$Game <- "Destiny"
names(destiny_text)[names(destiny_text) == "destiny_df$text"] <- "Text"

wow_df <- search_tweets(q = "Warcraft", n = 5000,
                            lang = "en", since ='2018-01-01',
                            include_rts = FALSE)

wow_text <- as.data.frame(wow_df$text)
wow_text$Game <- "World of Warcraft"
names(wow_text)[names(wow_text) == "wow_df$text"] <- "Text"

ffxiv_df <- search_tweets(q = "FFXIV", n = 5000,
                            lang = "en", since ='2018-01-01',
                            include_rts = FALSE)

ffxiv_text <- as.data.frame(ffxiv_df$text)
ffxiv_text$Game <- "Final Fantasy XIV"
names(ffxiv_text)[names(ffxiv_text) == "ffxiv_df$text"] <- "Text"

game_texts <- rbind(destiny_text, wow_text, ffxiv_text)

game_tokens <- game_texts %>%
  unnest_tokens(word, Text) %>%
  anti_join(stop_words_rev)

game_bigrams <- game_texts %>%
  unnest_tokens(bigram, Text, token = "ngrams", n = 2)
  
game_bigrams_clean <- game_bigrams %>%
  separate(bigram, c("word1","word2"), sep = " ")

game_bigrams_clean <- game_bigrams_clean %>%
  filter(!word1 %in% stop_words_rev$word) %>%
  filter(!word2 %in% stop_words_rev$word)



####BIGRAM ANALYSIS#####

bigrams_destiny <- game_bigrams_clean %>% 
  filter(Game == "Destiny") 

destiny_bigrams_count <- bigrams_destiny %>%
  count(word1, word2, sort = TRUE)

bigram_graph_destiny <- destiny_bigrams_count %>% 
  filter(n > 15) %>% 
  graph_from_data_frame()


set.seed(123)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph_destiny, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()+ggtitle("Network of Bigrams - Destiny")

bigrams_wow <- game_bigrams_clean %>% 
  filter(Game == "World of Warcraft") 

wow_bigrams_count <- bigrams_wow %>%
  count(word1, word2, sort = TRUE)

bigram_graph_wow <- wow_bigrams_count %>% 
  filter(n > 15) %>% 
  graph_from_data_frame()


ggraph(bigram_graph_wow, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()+ggtitle("Network of Bigrams - Wow")

bigrams_ffxiv <- game_bigrams_clean %>% 
  filter(Game == "Final Fantasy XIV") 

ffxiv_bigrams_count <- bigrams_ffxiv %>%
  count(word1, word2, sort = TRUE)

bigram_graph_ffxiv <- ffxiv_bigrams_count %>% 
  filter(n > 15) %>% 
  graph_from_data_frame()

ggraph(bigram_graph_ffxiv, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()+ggtitle("Network of Bigrams - FFXIV")

###Sentiment Analysis#####

afinn <- get_sentiments("afinn")

nrc <- get_sentiments("nrc")

bing <- get_sentiments("bing")

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon="nrc"),
                        mutate(bing, lexicon="bing")
)

tokens_destiny <- game_tokens %>% 
  filter(Game == "Destiny") 

  
destiny_bing_word_counts <- tokens_destiny %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

destiny_bing_word_counts

destiny_bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

destiny_afinn <- tokens_destiny %>% 
  inner_join(get_sentiments("afinn")) %>%  
  mutate(method = "AFINN") %>%
  count(word,value, sort = TRUE) %>%
  ungroup()
  

destiny_afinn %>%
  group_by(value) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

destiny_bing_and_nrc <- bind_rows(
  tokens_destiny %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tokens_destiny%>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative) 

destiny_afinn_sentiment <- destiny_afinn %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN") 

bind_rows(destiny_bing_and_nrc, destiny_afinn_sentiment) %>%
  ggplot(aes(method, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_x") +
  ggtitle("Sentiment Analysis - Destiny")


tokens_ffxiv <- game_tokens %>% 
  filter(Game == "Final Fantasy XIV") 


ffxiv_bing_word_counts <- tokens_ffxiv %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

ffxiv_bing_word_counts

ffxiv_bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

ffxiv_afinn <- tokens_destiny %>% 
  inner_join(get_sentiments("afinn")) %>%  
  mutate(method = "AFINN") %>%
  count(word,value, sort = TRUE) %>%
  ungroup()


ffxiv_afinn %>%
  group_by(value) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

ffxiv_bing_and_nrc <- bind_rows(
  tokens_ffxiv %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tokens_ffxiv%>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative) 

ffxiv_afinn_sentiment <- ffxiv_afinn %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN") 

bind_rows(ffxiv_bing_and_nrc, ffxiv_afinn_sentiment) %>%
  ggplot(aes(method, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_x") +
  ggtitle("Sentiment Analysis - Final Fantasy XIV")


tokens_wow <- game_tokens %>% 
  filter(Game == "World of Warcraft") 


wow_bing_word_counts <- tokens_wow %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

wow_bing_word_counts

wow_bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

wow_afinn <- tokens_wow %>% 
  inner_join(get_sentiments("afinn")) %>%  
  mutate(method = "AFINN") %>%
  count(word,value, sort = TRUE) %>%
  ungroup()


wow_afinn %>%
  group_by(value) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)

wow_bing_and_nrc <- bind_rows(
  tokens_wow %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  tokens_wow%>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive", 
                                         "negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative) 

wow_afinn_sentiment <- wow_afinn %>%
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN") 

bind_rows(wow_bing_and_nrc, wow_afinn_sentiment) %>%
  ggplot(aes(method, sentiment, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_x") +
  ggtitle("Sentiment Analysis - World of Warcraft")

#####TF-IDF

game_tokens_count <-  game_tokens %>%
  count(Game, word, sort=TRUE) %>%
  ungroup()

total_words <- left_join(game_tokens, game_tokens_count)%>%
  group_by(Game) %>%
  summarize(total=sum(n))

game_words <- left_join(game_tokens_count,total_words)


freq_by_rank <- game_words %>%
  group_by(Game) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

ggplot(freq_by_rank, aes(n/total, fill = Game))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.000009) +
  facet_wrap(~Game, ncol=2, scales="free_y")

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=Game))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()


game_tf_idf <- game_words %>%
  bind_tf_idf(word, Game, n)

game_tf_idf %>%
  arrange(desc(tf_idf))

game_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(Game) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=Game))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~Game, ncol=2, scales="free")+
  coord_flip()



