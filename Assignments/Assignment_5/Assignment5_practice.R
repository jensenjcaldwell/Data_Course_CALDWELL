library(tidyverse)
library(patchwork)
library(ggimage)
library(GGally)

data(iris)

ggpairs(iris)

iris %>% ggplot()


1:10 %>% sum()

iris %>% 
    filter(Species == "setosa" | Species == "virginica")%>%  
    ggplot(aes(x=Sepal.Length,y=Sepal.Width,color=Species))+
    geom_point()+
    facet_wrap(~Species) #divides it into two graphs



iris %>% 
  ggplot(aes(x=Sepal.Length,
             y=Petal.Length,
             color= Species))+
              geom_point(color="black")+
              geom_smooth(method = lm)+
              facet_wrap(~Species)+
              theme_bw()+
              theme(strip.text = element_text(face = "italic"),  #Strip text is facet titles
                    legend.position = "none") 

iris %>% str()

levels(iris$Species) <- c("virgininca", "versicolor", "Setosa") #didn't do what we wanted it to do

iris %>% 
  ggplot(aes(x=Species,
             y=Petal.Length))+
  geom_violin()


iris %>% 
    mutate(Sepal.Area = Sepal.Length * Sepal.Width,
           Species = factor(Species,levels = c("virginica","versicolor","setosa"))) %>% 
  ggplot(aes(x=Species,
             y=Petal.Length,
             fill=Species))+
  geom_violin()



iris %>% 
  group_by(Species) %>% 
  summarize(max_sep_len = max(Sepal.Length),
            min_sep_len = min(Sepal.Length),
            mean_sep_len = mean(Sepal.Length))
