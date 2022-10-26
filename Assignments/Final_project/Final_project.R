library(tidyverse)
library(modelr)
library(janitor)

df <- read_delim("Data/suicide_with_demo.txt") %>% clean_names()

df <- df[1:868,]



df %>% ggplot(aes(
  x=weekday,
  y=deaths
))+
  geom_boxplot()

names(df)

dd <- df %>% group_by(weekday_code) %>%
  mutate(day_deaths = mean(deaths)) %>% 
  select(weekday = weekday_code,
         day_deaths = day_deaths) %>% 
  filter(weekday < 8) %>% 
  mutate(weekday = as.double(str_replace(weekday,"1","8"))-1)
  
  

mod1 <- glm(dd, formula = day_deaths ~ weekday, family = gaussian)

summary(mod1)

ggplot(dd,
       aes(x=weekday,
           y=day_deaths))+
  geom_smooth(method = "glm")+
  geom_point()



cdf <- read_delim("Data/Suicide rates by county (1).txt") %>% clean_names() %>% mutate (county = str_remove(county, ", UT"))

cdf %>% arrange(crude_rate) %>% view
ggplot(aes(x=county,
                   y=crude_rate))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

rel <- read_csv("Data/Utah_Religions_by_County.csv") %>% clean_names() %>% select("county", "lds")

rel <- full_join(cdf,rel)

rel %>% filter(population > 1500000) %>% 
glm(formula = crude_rate ~ lds, family= "gaussian") %>% 
  summary()


rel %>%
  filter(population > 150000) %>% 
  ggplot(aes(x=lds, y=crude_rate, label = county))+
  geom_point()+
  geom_label()+
  geom_smooth(method = "lm")+
  labs(y="Suicide Rate By County", x="Proportion of County That Is LDS")


