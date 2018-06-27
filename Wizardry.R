library(harrypotter)
library(tidyverse)
library(tidytext)
library(quanteda)
library(textclean)
library(janitor)
library(quanteda.dictionaries)
library(psych)
library(GPArotation)
library(MASS) 
library(tm)
library(SnowballC)
stop <- rbind(tibble(text = stopwords("SMART")),"ill","im","yeah","dont","hey","back","lets")

titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

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
         book_factor = factor(book_list))

books <- books %>%
  group_by(book_factor) %>%
  mutate(chapter = row_number()) %>% 
  filter(!is.na(value)) 

token <- books %>%
  mutate(text = removePunctuation(value)) %>%
  unnest_tokens(text,text) %>%
  anti_join(stop) %>%
  dplyr::select(-value) %>%
  group_by(text) %>%
  count(text,sort = TRUE) %>%
  filter(n > 3)



liwc_book <- liwcalike(books$value, dictionary = data_dictionary_NRC) %>%
  mutate(linenumber = row_number())

full_liwc <- inner_join(books, liwc_book)

names(full_liwc)

test_pca <- full_liwc %>%
  ungroup %>%
  dplyr::select(-Parenth, -book_list,-value,-linenumber,-book_factor,-chapter,-docname,-Segment)

