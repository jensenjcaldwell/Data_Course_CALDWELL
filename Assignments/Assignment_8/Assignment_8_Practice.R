library(tidyverse)
library(janitor)
library(easystats)
library(palmerpenguins)
library(broom)

list("a",2,TRUE,c(1,2,3))

x <- list(sentences=sentences,letters=letters,numbers=1:10,iris=iris,whatever=TRUE)

x$sentences

purrr::map(
  
  
)


map(x,1)
map_chr(x,1)
map(x,1) %>% map(1)

data(sentences)

str_split(sentences," ") %>% 
  map_chr(1) %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

map(dfs,names)
map(dfs,dim)

files <- list.files("../../Data", 
                    recursive = TRUE, 
                    full.names = TRUE, 
                    pattern = "adjustment")

read_clean <- function(x){read_csv(x) %>% clean_names()}

dfs <- map(files,read_clean)

df <- reduce(dfs,full_join)


x

x %>% names()
x %>% length()
x[[1]][720]
x[[4]][1,1]

data(sentences)
clean_up_commas_etc <- function(x){
  x %>% str_remove("\\,") %>% 
    str_remove("\\.")
}

sentences %>% 
  str_split(" ") %>%
  map(str_to_lower) %>% 
  map(clean_up_commas_etc) %>% 
  unlist() %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq)) %>% 
  head()



mpg %>% 
  ggplot(aes(x=displ,y=cty,color=drv))+
  geom_point()+
  geom_smooth(method="lm")+
  theme_classic()

mod1 <- mpg %>% glm(data=.,
            formula=cty~displ+drv)

summary(mod1)

newdata <- data.frame(drv=factor('f'),
                      displ = 5)

predict(mod1,newdata)

library(modelr)

add_predictions(mpg,mod1) %>% 
  mutate(difference=abs(pred-cty)) %>% view()





df <- readRDS("../../Data/Lehi_Home_Sales.RDS")

df %>% ggplot(aes(x=acreage, y=rl_value))+
  geom_smooth()

max(df$rl_value)

read_clean <- function(x){
x %>% 
read_csv() %>% janitor::clean_names() %>% 
  mutate(species = str_replace(species,"P.","P. "))
}


read_clean("./Data/mushroom_growth.csv")
?map()
?reduce()


df <- data.frame(d50 = c(0,1,3,1,0), d70 = c(1,2,2,0,0), d90 = c(4,3,6,3,4))
df <- df %>% pivot_longer(cols = starts_with("d"), names_to = "group", values_to = "score") 

aov.df <- aov(formula = score ~ group,data = df) 

aov.df %>% effectsize::effectsize()

aov.df %>% summary()

aov.df %>% tidy()

aov.df %>% report()

TukeyHSD(aov.df) %>% tidy()

eta_squared(aov.df)

aov.df %>% report()

TukeyHSD(aov.df) %>% tidy()

?aov()


df %>% ggplot(aes(x=group, y=score))+
  geom_violin(fill="lightblue")+
  theme_minimal()+
  labs(y="# Of Errors", x="Temperature (F)")


effectsize::eta_squared(df)


ssdf <- data.frame()


dd <- read_delim("./Data/DatasaurusDozen.tsv")

dd <- dd %>% 
  group_by(dataset) %>% 
  mutate("mean_x"= mean(x), "mean_y"=mean(y))

  
ggplot(data=dd,aes(x=x,y=y))+
  geom_point()+
  facet_wrap(~dataset)
  



