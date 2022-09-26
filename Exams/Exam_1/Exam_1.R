library(tidyverse)
library(GGally)

#Step 1

df <- read.csv(file="cleaned_covid_data.csv") #reads the .csv to a dataframe
df$Last_Update <- as.Date(df$Last_Update) #THIS WAS INFURIATING TO FIGURE OUT!

#Step 2

A_states <- df[grepl("^A", df$Province_State),] #filters to only rows with States that start wit A and stores that as "A_states"

#Step 3

A_states %>% 
  ggplot(aes(x=Last_Update,
             y=Deaths,
             color=Province_State))+
  geom_point()+
    geom_smooth(se = FALSE)+
  facet_wrap(~Province_State, scales="free") #plots deaths over time

#Step 4



df %>%
  group_by(Province_State) %>% 
  summarize(Maximum_Fatality_Ratio=max(Case_Fatality_Ratio,na.rm = TRUE)) -> state_max_fatality_rate #creates new dataframe with the max for each state's fatality ratio

attach(state_max_fatality_rate) #attaches columns together
state_max_fatality_rate <- state_max_fatality_rate[order(Maximum_Fatality_Ratio, decreasing = TRUE),] #sorts in descending order




state_max_fatality_rate$Province_State <- #Coerces column into an ordered factor so geom_bar doesn't sort it 
                                factor(state_max_fatality_rate$Province_State,
                                levels=state_max_fatality_rate$Province_State)


#Step 5


state_max_fatality_rate %>% 
  ggplot(aes(x=Province_State,
             y=Maximum_Fatality_Ratio))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90,hjust = 0))


#Step 6 BONUs

attach(df)


df <- df[order(Last_Update, decreasing = FALSE),] #sorts in ascending order


df %>%  # plots cumulative for entire US
  mutate(
    total_deaths = cumsum(Deaths)
  ) %>% 
  filter(Province_State=="Wyoming") %>% 
  ggplot(aes(x=Last_Update,
         y=total_deaths))+
  geom_smooth(se=FALSE)
  
  





            
