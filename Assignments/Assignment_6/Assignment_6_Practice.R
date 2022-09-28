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

               