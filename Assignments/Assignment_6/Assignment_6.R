library(tidyverse)
library(GGally)
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


dat %>% 
  pivot_longer(cols = starts_with("HR"), names_to = "Time") %>% 
  filter(Substrate == "Itaconic Acid") %>% 
  ggplot(
    aes(x=Time,
        y=Dilution,
        color = `Sample ID`))+
  geom_smooth(se = FALSE)+
  facet_wrap(~Dilution)+
  theme_minimal()

