library(tidyverse)
library(ggimage)
library(jpeg)
library(ggpubr)

df <- read.csv("Data/president_heights.csv")
df$Name <- factor(df$Name,levels=df$Name)
df$`Height (inches)`=as.numeric(df$`Height (inches)`)

glory <- readJPEG("Media/retro2.jpg")

glimpse(df)

df %>% 
  ggplot(
    aes(x=`Height`,
        y=factor(Name), color=Name, angle=1:45, radius=`Height`
        ))+
  background_image(glory)+
  geom_hex()+
  geom_point()+
  geom_spoke(linetype="123456")+
  geom_smooth()+
  
  
  labs(title = "Heights of US presidents in measurements of 1/12 ft.",caption = "jlsa" )+
  
  
  theme(
    
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 15, hjust = 0.25, colour="yellow",angle = 2),

    panel.background = element_rect(fill = 'navy', colour = 'navy', size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),
    
    axis.title.x = element_text(face = "bold.italic", color = "red",angle=5),
    axis.text.y = element_text(angle=190,hjust = .5, size = 7),
    axis.text = element_text(face = "italic", size = 10),
    axis.text.x.bottom = element_text(angle = 180), 
    
    
    legend.background = element_rect(fill = "orangered1"),
    legend.key = element_rect(fill = "orangered1",linetype = "123234"),
    legend.direction = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    legend.title = element_text(family = "serif", color = "white"),
    legend.text = element_text(family = "mono", face = "italic", colour = "orangered3")
    
  ) 

ggsave("UglyPlot.png",plot = last_plot(), path = "Assignments/UglyPlot/",height = 10, width = 10, dpi = 300)

?ggsave()
