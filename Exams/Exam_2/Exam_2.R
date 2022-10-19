library(tidyverse)
library(janitor)
library(modelr)
library(broom)
library(easystats)

#step 1 import data
raw <- read_csv("unicef-u5mr.csv")

#step 2 clean data
df <- raw %>% 
  pivot_longer(cols = starts_with("U5MR"), names_to = "year", values_to = "mortality_rate") %>% 
  mutate(year = as.numeric(str_remove(year,"U5MR.")))
  

#Step 3, plotting mortality rate by country over time 
df %>% ggplot(aes(x=year,
                  y=mortality_rate,
                  fill=CountryName))+
  geom_line( color = "black")+
  facet_wrap(~Continent)+
  theme(legend.position = "none")+
  theme_bw()+
  labs(y="Mortality Rate of Chilren Under 5 per 1000 Live Births")

#Step 4, save plot
  ggsave(plot =,"CALDWELL_Plot_1.png", dpi = 900)

#Step plot 5 mean mortality rate by continent
  
  df %>% 
    filter(!is.na(mortality_rate)) %>% 
    group_by(Continent,year) %>% 
    mutate(continent_rate = mean(mortality_rate,)) %>% 
    pivot_wider(names_from = CountryName, values_from = mortality_rate) %>% 
    ggplot(aes(x=year,
               y=continent_rate,
               color = Continent))+
    geom_line(size = 2)+
    labs(y = "Mean Mortality rate of children under 5 by continent")

  #step 6 save plot
  
  ggsave(plot =,"CALDWELL_Plot_2.png", dpi = 900)  

  #Step 7 Create 3 models
  
  mod1 <- glm(data = df, formula = mortality_rate ~ year)
  mod2 <- glm(data = df, formula = mortality_rate ~ year + Continent)
  mod3 <- glm(data = df, formula = mortality_rate ~ year * Continent)

  #Step 8 Compare the three models
  
  compare_performance(mod1,mod2,mod3)
  compare_performance(mod1,mod2,mod3) %>% plot
  
          #Model 3 is clearly the best, it has by far more predictive power and seems to be less complicated, it appears to be better in every way, which makes sense. It has more variables that are significant.
  
  #Step 9 plot the models
  
  
  mods <- list(mod1, mod2, mod3)
  names(mods) <-  c("model_1", "model_2", "model_3")
  

  for (i in 1:length(mods)){
 df_pred <-  add_predictions(df, mods[[i]],var = names(mods[i]))
  }
  
  

  