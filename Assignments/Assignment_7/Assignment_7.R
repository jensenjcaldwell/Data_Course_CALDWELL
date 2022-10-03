library(tidyverse)

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


