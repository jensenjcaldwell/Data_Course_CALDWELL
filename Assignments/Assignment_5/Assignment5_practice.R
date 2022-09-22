library(tidyverse)
library(patchwork)
library(ggimage)
library(GGally)
library(palmerpenguins)

data(iris)

ggpairs(iris)

iris %>% ggplot()

str(df)
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


data(iris)


fat_flowers <- 
iris %>% 
  mutate(c1 = Sepal.Length > 5.8, #adds columns
         c2 = Sepal.Width >3,
         c3 = Petal.Length > 3.7,
         c4 = Petal.Width > 1.2,
         conditions = c1+c2+c3+c4) %>% 
  filter(conditions >= 2) %>% #filters rows
  select(-starts_with("c"))   #filters columns eliminating all that start with a c

fat_flowers %>% 
  ggplot(aes(x=Sepal.Width,
             y=Petal.Width,
             color = Species))+
  geom_blank()+
  geom_point(data=fat_flowers)


#penguins %>% GGally::ggpairs()

penguins %>% names()

penguins %>% 
  filter(!is.na(sex)) %>% #filters our NA values
  ggplot(aes(x=sex,
             y=body_mass_g,
             fill=sex))+
  geom_boxplot()+
  facet_wrap(~island)+
  scale_fill_manual(values=c("blue","green"))+
  theme_bw()+
  theme(strip.background = element_rect(fill=2,color="yellow",size=4),
        strip.text = element_text(face = "bold"))


library(ggimage)
# any local file path or image URL (using URL here for simplicity)
pic <- "https://cdn.icon-icons.com/icons2/1954/PNG/512/burger_122704.png"
p <- iris %>% 
  ggplot(aes(x=Sepal.Length,y=Petal.Length)) +
  theme_minimal()
p + 
  geom_image(image=pic) # not in aes()
# Could add a column of image paths if you want different images for different "groups"
# find 3 images (URL, in this case)
pic1 <- "https://static.thenounproject.com/png/2565215-200.png"
pic2 <- "https://static.thenounproject.com/png/703110-200.png"
pic3 <- "http://www.clker.com/cliparts/t/X/X/H/f/U/wrench-md.png"
# png files with transparent backgrounds work best
# make a vector of those paths that correspond to the iris species
iris$URL <- c(rep(c(pic1,pic2,pic3),each=50)) # save as new column (for an aesthetic)
iris %>% head() # just to see how this looks in your data set
iris %>% 
  ggplot(aes(x=Sepal.Length,y=Sepal.Width)) +
  geom_image(aes(image=URL)) + # set that new character column as an aesthetic
  theme_minimal()



library(gganimate)

