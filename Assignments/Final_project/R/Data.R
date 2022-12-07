library(tidyverse)

sbs <- read_delim("./Data/suicide_by_state.txt") %>% janitor::clean_names()

rbs <- readxl::read_excel("./Data/Religious Characteristics of States Dataset Project - Demographics v. 2.0 (RCS-Dem 2.0), COUNTRIES ONLY.XLSX")

sbs <- sbs %>% filter(!is.na(state))

swd <- read_delim("./Data/suicide_with_demo.txt") %>% janitor::clean_names()

swd %>% group_by(gender,state) %>% 
  mutate("gendertotalstate" = sum(deaths)) %>% ggplot(aes(x=state,y=gendertotalstate))+geom_boxplot()

colnames(master)

master <- read_delim("./Data/Suicide_demos.txt") %>% janitor::clean_names() %>% select(notes,single_year_ages_code, x2013_urbanization, gender_code, race, race_code, place_of_death, place_of_death_code, deaths) %>% 
  filter(is.na(notes))

master <- master %>% group_by(single_year_ages_code) %>% mutate("deaths_by_age" = sum(deaths))

master <- master %>% group_by(gender_code) %>% mutate("deaths_by_gender" = sum(deaths))

shinyuieditor::launch_editor(app_loc = "app/")
