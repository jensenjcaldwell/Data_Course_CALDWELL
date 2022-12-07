library(tidyverse)
library(janitor)
library(broom)
library(easystats)

# step 1: read, clean and match the plot

df <- read_csv("FacultySalaries_1995.csv") %>% janitor::clean_names()
df2 <- df
df3 <- df

df <- df %>% pivot_longer(cols = contains("salary"), names_to = "rank", values_to = "avg_salary") %>% 
  mutate(rank = str_remove(str_remove(str_remove(rank,"prof_salary_"),"avg_"),"_prof_salary")) %>% select(fed_id,univ_name,state,tier,rank,avg_salary)

df2 <- df2 %>% pivot_longer(cols = contains("comp"), names_to = "rank", values_to = "avg_comp") %>% 
  mutate(rank = str_remove(str_remove(str_remove(rank,"prof_comp_"),"avg_"),"_prof_comp")) %>% select(rank,avg_comp,fed_id)

df3 <- df3 %>% pivot_longer(cols = contains("num"), names_to = "rank", values_to = "num") %>% 
  mutate(rank = str_remove(str_remove(str_remove(rank,"num_"),"_profs"),"faculty_")) %>% select(rank,num,fed_id)


df_c <- full_join(df,full_join(df2,df3))

# all tidy, it's so BEAUTIFUL

df_c %>% 
  filter(tier != "VIIB", rank!="all") %>% 
  group_by(tier) %>% 
  ggplot(aes(y=avg_salary,fill = rank))+
  geom_boxplot()+
  facet_wrap(~tier)+
  theme_minimal()


# Step 2: ANOVA

aov <- df_c %>% aov(data=.,formula = avg_salary ~ state + tier + rank)

tidy(aov)

tidy(aov) %>% write.table(file = "Salary_ANOVA_Summary.txt")

#Step 3: more cleaning 


jo <- read_csv("Juniper_Oils.csv")

chem <- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene","beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene","compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol","gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol","compound 2","thujopsenal")

jo <- jo %>% pivot_longer(cols = chem, names_to = "compound", values_to = "concentration")

#step 4: plotting

jo %>% ggplot(aes(x= YearsSinceBurn, y=concentration))+
  geom_smooth()+
  facet_wrap(~compound, scales = "free")

#Step 5: GLM

jo2 <- jo %>% select(compound,concentration,YearsSinceBurn) %>% 
glm(data = ., formula = concentration ~ YearsSinceBurn * compound) %>% tidy()

jo2 %>% filter(p.value < .05) %>% mutate(term = str_remove(term,pattern = "compound"))






