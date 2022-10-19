library(tidyverse)
library(janitor)


raw <- read_csv("unicef-u5mr.csv")

df <- raw %>% 
  pivot_longer(cols = starts_with("U5MR"), names_to = "year", values_to = "mortality_rate") %>% 
  mutate(year = as.numeric(str_remove(year,"U5MR.")))
  
df %>% ggplot(aes(x=year,
                  y=mortality_rate,
                  fill=CountryName))+
  geom_line( color = "black")+
  facet_wrap(~Continent)+
  theme(legend.position = "none")+
  theme_bw()
