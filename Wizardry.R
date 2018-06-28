library(harrypotter)
library(tidyverse)
library(tidytext)
library(quanteda)
library(textclean)
library(janitor)
library(quanteda.dictionaries)
library(psych)
library(stm)
library(textstem)
library(topicmodels)
library(tm)
library(SnowballC)
library(drlib)
stop <- rbind(tibble(text = stopwords("SMART")),"ill","im","yeah","dont","hey","back","lets")

titles <- c("Book. 1 Philosopher's Stone", "Book. 2 Chamber of Secrets", "Book.3 Prisoner of Azkaban",
            "Book. 4 Goblet of Fire", "Book. 5 Order of the Phoenix", " Book. 6 Half-Blood Prince",
            "Book. 7 Deathly Hallows")

books <- list(
  philosophers_stone,
  chamber_of_secrets,
  prisoner_of_azkaban,
  goblet_of_fire,
  order_of_the_phoenix,
  half_blood_prince,
  deathly_hallows) %>%
  set_names(titles) %>%
  map_df(as.tibble, .id = "book_list") %>%
  mutate(linenumber = row_number(),
         book_factor = factor(book_list, levels = unique(titles))) %>%
  group_by(book_factor) %>%
  mutate(chapter = row_number()) %>% 
  filter(!is.na(value)) 

books <- rename(books, text = value)

token <- books %>%
  mutate(text = str_replace_all(text,"'s",""), 
         text = removePunctuation(text),
         text = replace_number(text)) %>%
  unnest_tokens(text,text) %>%
  anti_join(stop) %>%
  ungroup() %>%
  mutate(text = lemmatize_words(text))

remover <- token %>%
  count(text) %>%
  filter(n > 50)

token_clean <- inner_join(token, remover, by = "text")

text_tf_idf <- token_clean %>%
  count(book_factor, text, sort = TRUE) %>%
  bind_tf_idf(book_factor,text,nn) %>%
  arrange(-tf_idf) %>%
  group_by(book_factor) %>%
  top_n(10) %>%
  ungroup()

text_tf_idf %>%
  mutate(word = reorder_within(text, tf_idf, book_factor)) %>%
  ggplot(aes(word,tf_idf,fill = book_factor)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~book_factor, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text = element_text(size = 11)) +
  theme_bw() +
  labs(x = NULL, y = "tf_idf",
       title = "Highest word frequesncy")

test_sparse <- token_clean %>%
  filter(!text %in% c("ron","harry","hermione","dumbledore","voldemort","professor",
                      "hed","eye","head","didnt","mr","look",
                      "malfoy","weasley","make")) %>%
  count(book_factor, text, sort = TRUE)%>%
  cast_sparse(book_factor,text,nn)

topic_model <- stm(test_sparse, K = 9,
                   verbose = FALSE,
                   init.type = "Spectral")

td_test <- tidy(topic_model)
td_test %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

liwc_book <- liwcalike(books$text, dictionary = data_dictionary_NRC) %>%
  mutate(linenumber = row_number())

full_liwc <- inner_join(books, liwc_book)

names(full_liwc)

test_pca <- full_liwc %>%
  ungroup %>%
  dplyr::select(-Parenth, -book_list,-value,-linenumber,-book_factor,-chapter,-docname,-Segment)

