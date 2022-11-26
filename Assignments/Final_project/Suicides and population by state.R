library(tidyverse)
library(janitor)
library(gganimate)

df <- read_delim("Data/Suicide_sy.txt") %>% clean_names()

df <- df[1:1173,] %>% mutate(year %>% str_squish() %>% as.numeric())



  df %>% filter(is.na(notes)) %>%
    ggplot(aes(x=population, y=crude_rate))+
  geom_point()+
    transition_time(year_code)+
    labs(subtitle = "year_code is {frame}")

      ?filter()
  