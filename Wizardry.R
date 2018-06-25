devtools::install_github("bradleyboehmke/harrypotter")
pacman::p_load(tidytext,janitor,quanteda,tidyverse)
devtools::install_github("kbenoit/quanteda.dictionaries")
library(quanteda.dictionaries)

library(harrypotter)
options(max.print = 500)

stop <- rbind(tibble(text = stopwords(source = "smart")),"ill","im","yeah","dont","hey","back","lets")


books <- list(
  philosophers_stone = philosophers_stone,
  chamber_of_secrets = chamber_of_secrets,
  prisoner_of_azkaban = prisoner_of_azkaban,
  goblet_of_fire = goblet_of_fire,
  order_of_the_phoenix = order_of_the_phoenix,
  half_blood_prince = half_blood_prince,
  deathly_hallows = deathly_hallows) %>%
  set_names(titles) %>%
  map_df(as.tibble, .id = "book") %>%
  mutate(book_factor = factor(book, levels = c("philosophers_stone", "chamber_of_secrets",
              "prisoner_of_azkaban", "goblet_of_fire",
              "order_of_the_phoenix", "half_blood_prince",
              "deathly_hallows"))) %>%   
  group_by(book) %>%
  mutate(chapter = row_number(book))


clean_books <- books %>% 
  as.tibble() %>%
  janitor::clean_names() %>%
  mutate(text = removePunctuation(as.character(value)),
         work = case_when(book == "philosophers_stone" ~ "Philosophers Stone",
                           book == "chamber_of_secrets" ~ "Chamber of Secrets",
                           book == "prisoner_of_azkaban" ~ "Prisoner of Azkaban",
                           book == "goblet_of_fire" ~ "Goblet of Fire",
                           book == "order_of_the_phoenix" ~ "Order of the Phoenix",
                           book == "half_blood_prince" ~ "Half Blood Prince",
                           book == "deathly_hallows" ~ "Deathly Hallows",
                           )) %>% 
  unnest_tokens(text, text) %>%
  anti_join(stop)
  
liwc_book <- liwcalike(clean_books$value, dictionary = data_dictionary_NRC)
