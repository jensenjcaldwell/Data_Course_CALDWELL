library(tidyverse)
library(ggimage)
library(jpeg)
library(ggpubr)
library(showtext)
library(extrafont)
remotes::install_version("Rttf2pt1", version = "1.3.8")
font_import(path = "Media", recursive = TRUE)
loadfonts(device = "win", quiet = "FALSE")
windowsFonts()
font_add_google("Gochi Hand", "gochi")

font_install()

df <- read.csv("Data/president_heights.csv")
df$Name <- factor(df$Name,levels=df$Name)
df$Height=as.numeric(df$Height)

glory <- readJPEG("Media/retro2.jpg")

glimpse(df)

df %>% 
  ggplot(
    aes(x=`Height`,
        y=factor(Name), color=Name, angle=1:45, radius=`Height`
        ))+
  
  
  
  labs(title = "Heights of US presidents in measurements of 1/12 ft.",caption = "jlsa" )+
  
  
  theme(
    
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 15, hjust = 0.25, colour="yellow",angle = 2, family = "hand"),
    panel.background = element_rect(fill = 'navy', colour = 'navy', size = 4),
    panel.border = element_rect(fill = NA, color = "green", size = 2),
    panel.grid.major.x = element_line(color = "purple", linetype = 2),
    panel.grid.minor.x = element_line(color = "orange", linetype = 3),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_text(face = "bold.italic", color = "red",angle=5),
    axis.text.y = element_text(angle=190,hjust = .5, size = 7),
    axis.text = element_text(face = "italic", size = 10, color = "darkgreen"),
    axis.text.x.bottom = element_text(angle = 187), 
    legend.background = element_rect(fill = "orangered1"),
    legend.key = element_rect(fill = "orangered1",linetype = "123234"),
    legend.direction = "horizontal",
    legend.position = "top",
    legend.justification = "bottom",
    legend.title = element_text(family = "serif", color = "orange"),
    legend.text = element_text(family = "mono", face = "italic", colour = "orangered3", angle = 1),
    
  ) +
 
  background_image(glory)+
  geom_point()+
  geom_spoke(linetype="123456")+
  geom_smooth()



ggsave("UglyPlot.png",plot = last_plot(), path = "Assignments/UglyPlot/",height = 10, width = 10, dpi = 300)

?ggsave()
