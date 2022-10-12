library(tidyverse)
library(ggplot2)

df <- read_csv("Utah_Religions_by_County.csv")

keepers <- colnames(df[1:4])


df <- 
df %>% pivot_longer(cols = -all_of(keepers), names_to = "religion", values_to = "proportion" )


df %>% summary()

df %>% 
  ggplot(aes(x=religion, y=proportion))+
  geom_col()+
  facet_wrap(~County)+
  theme(
    axis.text.x = element_text(angle=90)
  ) #shows religious composition in each county

df %>% 
  ggplot(aes(x=County, y=Religious))+
  geom_col()+
  theme(
    axis.text.x = element_text(angle=90)
  ) #shows religiosity composition in each county



df %>% 
  filter(religion == "LDS") %>% 
  ggplot(aes(y=proportion, x=Pop_2010,label=County))+
  geom_point()+
  geom_label(label.size = .1, size=2)+
  geom_smooth(method="lm")+
  theme(
    axis.text.x = element_text(angle=90)
  ) #This plot compares county population to the proportion that is lds, surprisingly there seems to be no correlation between population and proportion of LDS individuals


df %>% 
  ggplot(aes(y=proportion, x=Pop_2010,label=County))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~religion)+
  theme(
    axis.text.x = element_text(angle=90)
  ) #This graph compares the population vs religion for all counties but this time for all religions, again there appears to be no correlation, which once again surprises me.


df %>% 
  ggplot(aes(y=`Non-Religious`, x=proportion,label=County))+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_wrap(~religion, scales = "free")
  theme(
    axis.text.x = element_text(angle=90)
  ) #This graph


df %>% ggplot































airlines <- read_csv("airlines.csv")
airport <-  read_csv("airports.csv")
flights <- read_csv("jan_flights.csv")
snow <- read_csv("Jan_snowfall.csv")

States <- 
  airport %>% 
  mutate(iata = IATA_CODE) %>% 
    select(iata,STATE)


flights$Date <- as.Date(with(flights,paste(YEAR,MONTH,DAY,sep = "-")),"%Y-%m-%d")

glimpse(flights)

flights <- 
flights %>% 
  select(AIRLINE,ORIGIN_AIRPORT,DEPARTURE_DELAY,Date) %>% 
  full_join(snow)


