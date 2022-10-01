library(tidyverse)
library(GGally)
library(gganimate)
dat <- read_csv("../../Data/BioLog_Plate_Data.csv")

names(dat)


dat %>% 
  pivot_longer(cols = starts_with("HR"), names_to = "time") %>% 
  mutate(sample_type = grepl("S.*" , `Sample ID`)) %>% 
  filter(Dilution == .1) %>% 
group_by(Well) %>% 
  summarize(Substrate = Substrate,
            Type = gsub("Clear_Creek", "Water",
                        gsub("Waste_Water" , "Water" ,
                             gsub("Soil_1", "Soil",
                                  gsub("Soil_2", "Soil", `Sample ID`)))),
            Time = as.numeric(gsub("Hr_","",time)),
            Absorbance = value
    
  ) %>% 
  ggplot(
    aes(x=Time,
        y=Absorbance,
        color = Type))+
  geom_smooth(se = FALSE)+
  facet_wrap(~Substrate)+
  theme_minimal()+
  labs(title = "Just dilution 0.1")






#



dat %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  pivot_longer(cols = starts_with("HR"), names_to = "time") %>% 
  pivot_wider(names_from = Rep, values_from = value) %>% 
  mutate(mean_absorbance = (`1`+`2`+`3`)/3) %>% 
   #view()
  group_by(Well) %>% 
  summarize(Substrate = Substrate,
            `Sample ID` = `Sample ID`,
            Time = as.numeric(gsub("Hr_","",time)),
            Dilution = Dilution,
            mean_absorbance=mean_absorbance
            
            
  ) %>% 
  ggplot(
    aes(x=Time,
        y=mean_absorbance,
        color = `Sample ID`))+
  geom_line()+
  facet_wrap(~Dilution)+
  theme_minimal()+
  transition_reveal(Time)
