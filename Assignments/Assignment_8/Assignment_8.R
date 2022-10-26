library(tidyverse)
library(modelr)
library(broom)
library(easystats)

df <- read_csv("../../Data/mushroom_growth.csv")


df %>% ggplot(aes(x=Nitrogen, y=GrowthRate, fill=Species))+
  geom_point()

df %>% ggplot(aes(x=Humidity, y=GrowthRate, fill=Species))+
  geom_boxplot()

df %>% ggplot(aes(x=factor(Temperature), y=GrowthRate))+
  geom_boxplot()

df %>% ggplot(aes(x=factor(Light), y=GrowthRate, fill=Species))+
  geom_boxplot()

mod1 <- glm(data = df, formula = GrowthRate ~ Species)
mod2 <- glm(data = df, formula = GrowthRate ~ Species + Humidity)
mod3 <- glm(data = df, formula = GrowthRate ~ Species + Light + Nitrogen + Humidity + Temperature)
mod4 <- glm(data = df, formula = GrowthRate ~ Species * Light * Nitrogen * Humidity * Temperature)

compare_performance(mod1,mod2,mod3,mod4)

compare_performance(mod1,mod2,mod3,mod4) %>% plot
#mod4 is the best in every way, see plot if you don't believe me

add_predictions(data.frame(Species = "P.ostreotus", Light = 15, Nitrogen = 50, Humidity = "Low", Temperature = 35),mod4) #seems like a possible actual growth rate

add_predictions(df,mod4) %>% pivot_longer(cols = c(pred,GrowthRate), names_to = "data_type", values_to = "stat") %>% 
  ggplot(aes(x=Species, y = stat, fill=data_type))+
  geom_violin()


