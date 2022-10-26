library(tidyverse)
library(janitor)
library(easystats)
library(palmerpenguins)

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



