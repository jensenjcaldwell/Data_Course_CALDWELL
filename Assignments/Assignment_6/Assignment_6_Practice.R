library(tidyverse)
library(readxl)

#rules of data
#every column is a single variable
#every row is a single observation of those variables
#must be rectangular (missing cells have NA)
#


df <- read_csv("./Data/wide_income_rent.csv")



#wide format means that some variables are spread across multiple columns
#rows and columns are essentially swapped

df2 <- read_xlsx("./Data/wide_data_example.xlsx")


df2 %>% 
  mutate(`Treatment 1` = `Treatment 1` %>% as.numeric()) %>% 
  pivot_longer(cols = starts_with("Treatment"), names_to = "Treatment", values_to = "Weight") %>%
  ggplot(aes(x=SampleID,
             y=Weight,
             color=Treatment
             ))+
  geom_point()+
  scale_fill_viridis_d("C")


 df %>% 
  pivot_longer(cols = -variable, names_to = "State") %>% #Turns the state column names into entries into a new column named "State"
   pivot_wider(names_from = variable) 

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
table1

table2 %>% pivot_wider(names_from = type, values_from = count)


table4a %>% view()
table4b %>% view()


table4a %>% pivot_longer(names_to = "year", values_to = "cases", cols = -country)

table4b %>% 
  pivot_longer(names_to = "year", values_to = "population", cols=-country)
  

full_join(table4a %>% pivot_longer(names_to = "year", values_to = "cases", cols = -country),
          table4b %>% 
            pivot_longer(names_to = "year", values_to = "population", cols=-country))



#


library(carData)

MplsStops %>% view()

MplsDemo %>% view()


MplsStops %>% ggplot(
  aes(
    x=long,
    y=lat,
    color=race))+
  geom_density_2d()
  

MplsStops$citationIssued

full <- full_join(MplsStops,MplsDemo)


names(full)



full %>% 
  group_by(neighborhood) %>% 
  summarize(N=n(),
            N_per_pop = N / population,
            white=white,
            black=black,
            foreignBorn=foreignBorn,
            hhIncome=hhIncome) %>% 
  unique.data.frame() %>% 
  arrange(desc(N_per_pop)) %>% 
  ungroup() %>% 
  ggplot(aes(
    x=hhIncome,
    y=N_per_pop,
    label=neighborhood))+
  geom_point()+
  geom_smooth(method = "lm")
  