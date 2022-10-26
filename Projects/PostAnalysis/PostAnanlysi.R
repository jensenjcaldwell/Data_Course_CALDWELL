library(tidyverse)
library(modelr)
library(easystats)

files <- list.files("Projects/PostAnalysis/", full.names = TRUE)
big <- read_csv(files[1])
small <- read_csv(files[2])


df <- left_join(small,big, by = "text")


df <- df %>% pivot_longer(cols = contains("_"),
                    names_to = "stat_type",
                    values_to = "stat") %>% 
     mutate(gn = case_when(gn == "GN" ~ TRUE,
                            gn == "C" ~ FALSE,
                            TRUE~FALSE))
df %>% ggplot(aes(x=gn,
                  y=stat,
                  fill=gn))+
  geom_boxplot()+
  facet_wrap(~stat_type, scales = "free")+
  theme_bw()+
  labs(y="Ratio",title = "Comparison of Gendered and Gender-Neutral Across Like and Retweet Ratios", x = "Gender Neutral T/F") 

ggsave("Projects/PostAnalysis/Plot.png", dpi = 900)


df2 <- df %>% 
  filter (stat_type == "interaction_ratio") %>% 
  select(ratio = stat,
         gn = gn
         ) 
df2 <- df2 %>% mutate(ratio = as.double(ratio))

Ss
  
mod1 <- glm(data = df2, formula = ratio ~ gn)

summary(mod1)

report(mod1)


