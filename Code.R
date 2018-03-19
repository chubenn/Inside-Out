pacman::p_load(tidyverse,tm,tidytext,ggplot2,quanteda,stm)

stop <- tibble(text = stopwords("SMART"))
nrc <- sentiments %>% filter(lexicon == "nrc") %>% select(-score, -lexicon)
text_cleanish <- read.csv("~/GitHub/Inside-Out/text_cleanish.csv", header=TRUE) %>%
  mutate(line = row_number())

reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
  new_x <- paste(x, within, sep = sep)
  stats::reorder(new_x, by, FUN = fun)
}

scale_x_reordered <- function(..., sep = "___") {
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}

##cleaning

text.dat <- text_cleanish %>% 
  as.tibble() %>%
  janitor::clean_names() %>%
  mutate(text = removePunctuation(as.character(text))) %>% 
  unnest_tokens(text, text) %>%
  anti_join(stop)

text.dat %>%
  count(text, sort = TRUE)
speaker_count <- text.dat %>%
  count(speaker, sort = TRUE)

text.dat.1 <- text.dat %>%
  filter(speaker == list("Joy","Sadness","Bing Bong","Fear","Anger",
                         "Disgust","Mom","Riley","Dad"))

##Structural stuff
speaker_tf_idf <- text.dat.1 %>%
  count(speaker, text, sort = TRUE) %>%
  bind_tf_idf(text, speaker, n) %>%
  arrange(-tf_idf) %>%
  group_by(speaker) %>%
  top_n(10) %>%
  ungroup() 

speaker_tf_idf %>%
    mutate(word = reorder_within(text, tf_idf, speaker)) %>%
    ggplot(aes(word, tf_idf, fill = speaker)) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ speaker, scales = "free", ncol = 3) +
    scale_x_reordered() +
    coord_flip() +
    theme(strip.text=element_text(size=11)) +
    labs(x = NULL, y = "tf-idf",
         title = "Highest tf-idf words",
         subtitle = "Individual stories focus on different narrative elements")

inside_sparse <- text.dat %>%
  count(speaker,text, sort = TRUE) %>%
  cast_sparse (speaker,text,n)

topic_model <- stm(inside_sparse, K = 6, verbose = FALSE, init.type = "Spectral")
summary(topic_model)
td_inside <- tidy(topic_model)

td_inside %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")

##sentiment analysis first
sent.join <- text.dat %>%
  inner_join(nrc, by = c("text" = "word")) 

sent.speaker <- text.dat %>%
  inner_join(nrc, by = c("text" = "word")) %>%
  count(speaker, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive-negative, 
         total = anger + anticipation + disgust + fear + joy + 
           sadness + surprise + trust + negative + positive,
         ratio_positive = (anticipation + joy + surprise + trust + positive)/total) %>%
  arrange(desc(ratio_positive))

sent.join %>%
  count(text, sentiment) %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(text, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip()
