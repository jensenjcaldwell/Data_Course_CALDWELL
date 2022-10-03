library(tidyverse)
library(carData)
library(readxl)


df <- full_join(MplsStops,MplsDemo)
glimpse(df)

GGally::ggpairs(df)

df_sum <- 
df %>% 
  group_by(neighborhood) %>% 
  summarise(N=n(),
            stops_per_person = N/population,
            collegeGrad = collegeGrad,
            hhIncome = hhIncome,
            white=white,
            black=black,
            foreignBorn=foreignBorn,
            poverty=poverty,
            vehicleSearchTotal = sum(vehicleSearch),
            percent_searched = vehicleSearchTotal/N,
            proportionSuspicious = sum(suspiciousSearch)/N)

df_sum %>% 
  unique.data.frame() %>% 
  ggplot(aes(x=poverty,y=percent_searched,color=proportionSuspicious))+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_color_viridis_c()








levels(df$problem)
levels(df$citationIssued)
levels(df$personSearch)

df$citationIssued %>% table()
df$citationIssued %>% summary()

df <- df %>% 
  mutate(citationIssued = case_when(citationIssued == "YES" ~ TRUE,
                                    citationIssued != "YES" ~ FALSE,
                                    TRUE ~ FALSE),
         vehicleSearch = case_when(vehicleSearch == "YES" ~ TRUE,
                                   T~F),
         suspiciousSearch = case_when(problem == "suspicious" & vehicleSearch == TRUE ~ TRUE,
                                      TRUE ~ FALSE)
         )




df %>% 
  ggplot(aes(x=long, y=lat, color=suspiciousSearch))+
  geom_point()


library(palmerpenguins)

p <- penguins


p %>% 
  mutate(chonker = case_when(body_mass_g > 4000 ~ TRUE,
                             TRUE~FALSE)) %>% 
  ggplot(aes(x=flipper_length_mm, y=body_mass_g, color = chonker))+
  geom_point()




p %>% 
  filter(!is.na(sex)) %>% 
  mutate(size = case_when(body_mass_g >= 5000 ~ "Fattie",
                          body_mass_g >= 4000 & body_mass_g < 5000 ~ "Medium",
                          body_mass_g < 4000 ~ "Tiny",
                          )) %>% 
  ggplot(aes(x=body_mass_g, fill=size))+
  geom_density(alpha=.5)+
  facet_wrap(~species*sex)








df <- read_xlsx("../../Data/messy_bp.xlsx")

df %>% 
  setNames(c("pat_id","month","BP1","HR1","BP2","HR2","BP3","HR4"))
  
