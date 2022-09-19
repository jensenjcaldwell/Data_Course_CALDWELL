library(tidyverse)
library(patchwork)

df <- read.csv("Data/cdc_scores.csv")

x <- df$YEAR == "2005"
y <-  df$YEAR == "2020"

xx <- df[x,]
yy <- df[y,]

X <- ggplot(xx,
            aes(x=STATE,
                y=RATE))+
                geom_point(color="cyan")+
                labs(title = "2005")
Y <- ggplot(yy,
            aes(x=STATE,
                y=RATE))+
  geom_point(color="red")+
  labs(title = "2020")

X/Y

ggsave("CDCscores.png", path="Assignments/Assignment_4/")
