#learning about functions
library(tidyverse)

x <- 1:10
sum(x)

my_sum <- function(x){sum(x,na.rm = TRUE)}

y <- c(1:3,NA)

sum(y)
my_sum(y)

mpg

fun1 <- function(mpg){ggplot(mpg, aes(x=displ,y=cty))+ geom_point()}

fun1(mpg)

data(mpg)

fun2 <- function(x,var1,var2){
  library(tidyverse)
  if(!is.data.frame(x)){stop("x must be a dataframe, you moron")}
  if(!is.character(var1)){stop("var1 is not a character, you idiot!")}
  if(!is.character(var2)){stop("var2 is not a character, you idiot!")}
  
  
   ggplot(x, aes(x=x %>% pluck(var1),
                                               y=x %>% pluck(var2)))+
    geom_point()+
    labs(x=var1,
         y=var2)+
    theme_minimal()}



fun2(mpg,"hwy","cty")

data(iris)

fun2(iris,"Sepal.Length","Sepal.Width")




x <- 1:10


tabulatr <- function(x){
  if(!is.numeric(x)){"x must be a numeric vector"}
  abs(x-min(x))}

tabulatr(iris$Sepal.Width)



df <- read_csv("./Data/Utah_Religions_by_County.csv") %>% janitor::clean_names()

df <- df %>% 
  pivot_longer(cols = !c("county","pop_2010", "religious", "non_religious"), names_to = "religion", values_to = "proportion")

max_religion <- function(dat, county){
  dat %>% 
    filter(county == county) %>% 
    filter(proportion == max(proportion)) %>% 
     pluck("religion") -> z
    "The dominant religion in"county"is"z
          
          }
  

max_religion(df,"Beaver County")








gm_auth_configure()

library(gmailr)

gm_threads()


gm_mime() %>% 
  gm_to("mypretendmail1@gmail.com") %>% 
  gm_from("jensenjcaldwell@gmail.com") %>% 
  gm_cc("jensenjcaldwell@gmail.com") %>% 
  gm_subject("This is a test") %>%
  gm_text_body("I am testing the gmailr package")  %>% 


  gm_send_message()``
  
gm_create_draft()

gm_auth_configure(key = "708745422704-iguok75k32jr9io3ag62orbm53g6o8e5.apps.googleusercontent.com", secret = "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")

gm_auth

sentences

