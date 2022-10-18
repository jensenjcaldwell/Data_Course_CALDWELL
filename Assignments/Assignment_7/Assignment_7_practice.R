library(tidyverse)
library(carData)
library(readxl)
library(janitor)


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
  





#




lds <- read_csv("../../Data/LDSByState.csv")
rates <- read_csv("../../Data/SuicideByState.csv")
states <- read_csv("../../Data/USStates.csv")

r2020 <- 
  rates %>% 
  filter(YEAR == 2015) %>% 
  mutate(Code = STATE)

lds <- full_join(lds,states) 

data <- full_join(lds,r2020)


data %>% 
  ggplot(
    aes(x=mormonRate,
        y=RATE))+
      geom_point()+
      geom_smooth(method = "lm")
  )


cor.test(x = data$mormonRate,y = data$RATE)


#




df <- read_xlsx("../../Data/messy_bp.xlsx",skip = 3) %>% clean_names()


#gets list of visits
extranames <- read_xlsx("../../Data/messy_bp.xlsx", skip = 2, n_max = 1) %>% 
  select(starts_with("visit ")) %>% 
  names() %>% make_clean_names()

bp <- df %>% #Blood pressur to dataframe
  select(starts_with("bp_"))
hr <- df %>% #Heart rate to dataframe
  select(starts_with("hr_"))
pat <- df %>% #patient data to dataframe
  select(-starts_with("bp_")) %>% 
  select(-starts_with("hr_"))

paste0(pat$year_birth,"-",pat$month_of_birth,"-",pat$day_birth) %>% as.Date()
           
pat <- pat %>% 
  mutate(birthdate = as.Date(paste0(pat$year_birth,"-",pat$month_of_birth,"-",pat$day_birth))) %>% 
  select(-contains("_birth")) %>% 
  mutate(pat_id = 1:nrow(.)) %>% 
  mutate(hispanic = case_when(hispanic == "Hispanic" ~ TRUE,
                              TRUE ~ FALSE)) %>% 
  mutate(race = race %>% str_to_lower() %>% 
           str_replace("caucasian","white"),
         sex=sex %>% str_to_lower())

#renaming columns
names(bp) <- extranames
names (hr) <- extranames

bp <- 
pat %>% bind_cols(bp) %>% pivot_longer(starts_with("visit_"),names_to = "visit", values_to = "bp") %>% 
  separate(bp, into = c("systolic", "diastolic"), convert = TRUE)

hr <- 
  pat %>% bind_cols(hr) %>% pivot_longer(starts_with("visit_"),names_to = "visit", values_to = "hr")

df <- full_join(bp,hr) %>% 
  mutate(visit = visit %>% str_remove("visit_") %>% as.numeric())

saveRDS(df,"../../Data/CleanBP.RDS")

write_csv(df,file = "../../Data/CleanBP.csv")

