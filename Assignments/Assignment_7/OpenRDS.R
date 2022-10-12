library(tidyverse)
df <- readRDS("../../Data/CleanBP.RDS")

df %>% ggplot(
  aes(x=visit,
      y=systolic,
      color=race)
)+
  geom_point()+
  geom_smooth()

df %>% 
  lm(data=., formula = systolic ~ visit) %>% summary
