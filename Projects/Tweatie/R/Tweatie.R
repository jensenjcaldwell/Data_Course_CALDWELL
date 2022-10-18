library(rtweet)
library(tidyverse)
library(readxl)

rtweet::auth_setup_default()

twitter_token <- create_token(
  app = "Tweatie",
  consumer_key = "DIvYEdkENbbc2fDEgxi1AEgC2",
  consumer_secret = "ycZ6xsNrHHs0y1opJsdiHUunFdLKzdJzgBd3euerGJ8BGJD494",
  access_token = "555079478-555079478-w6LkuxNctK8TrKZVxh0RVUqPVBhhJsp8hdK5lx8b",
  access_secret = "xy4DhMENpZMl9dzaxND2MG20vr0vhORoTPpTKllBPhnrZ",
  set_renv = TRUE)

climate <- search_tweets("climate", n=10, include_rts=FALSE, lang="en",)

auth <- rtweet_bot()

auth_as(twitter_token)

i <- 
#

accounts <- c("@voguemagazine","@MensHealthMag","@Dove","@Cosmopolitan","@Gillette")

for(i in accounts){
Account <- i

filepath = paste("Projects/Tweatie/Output/",str_to_lower(str_remove(Account,"@")),".csv",sep = "")

profile <- search_users(Account,n = 1)

get_timeline(Account, n=5000) %>% 
  filter(is.na(in_reply_to_status_id_str)) %>% 
  select(date=created_at, 
         text = full_text,
         id=id_str,
         link = source,
         retweets = retweet_count,
         likes = favorite_count) %>% 
          mutate(account = Account,
                 followers = profile$followers_count[1],
                 like_ratio = likes/followers,
                 retweet_ratio = retweets/followers,
                 interaction_ratio = sum(retweets,likes)/followers
                 ) %>% 
  write.csv(file = filepath)

}

files <- list.files("Projects/Tweatie/Output/",full.names = TRUE)
main <- read_csv(files[1])

for(i in files){
  
  df <- read_csv(i,col_types = c("n","T",""))
  df2 <- read_csv(i,col_select = "id", guess_max = 0)
  df <- mutate(df, id2 = as.character(df2$id))
  main <- full_join(main,df)
}



glimpse(main)

main <- main %>% mutate(link= paste(sep = "","https://twitter.com/",str_remove(account,"@"),"/status/",id2))



write.csv(main,"Projects/Tweatie/Output/main.csv")



