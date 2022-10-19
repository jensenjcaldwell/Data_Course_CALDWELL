library(tidyverse)
library(janitor)
library(easystats)
library(palmerpenguins)
library(modelr)
library(broom)

penguins
names(penguins)
#Response variable is body_mass_g
#predictors: species, island, sex

penguins %>%
  filter(!is.na(sex)) %>% 
  ggplot(aes(y=body_mass_g, 
                        x=species,
                        fill = sex,
                        ))+
  geom_boxplot()+
  facet_wrap(~island)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#formula = response variable ~ predictors

penguins %>%
  filter(!is.na(sex)) %>% 
  glm(data = .,
      formula = body_mass_g ~ sex + island + species) -> mod1

summary(mod1)


tidy(mod1)

tidy(mod1) %>% 
  filter(p.value < .05)


penguins %>%
  filter(!is.na(sex)) %>% 
  glm(data = .,
      formula = body_mass_g ~ sex + species) -> mod2

tidy(mod2)

tidy(mod2) %>% filter(p.value<.05)

report(mod2)

#ML = maximum likelihood


model_performance(mod2)

compare_performance(mod1,mod2)


penguins %>%
  filter(!is.na(sex)) %>% 
  glm(data = .,
      formula = body_mass_g ~ sex * species) -> mod3

compare_performance(mod1,mod2,mod3) %>% plot

mod4 <-
  glm(data =  penguins %>%
        filter(!is.na(sex)),
      formula = body_mass_g ~ .^2)

step <- MASS::stepAIC(mod4)

step$formula
 
summary(mod4)

mod5 <- glm(data = penguins %>% filter(!is.na(sex)),
            formula = step$formula)

compare_performance(mod1,mod2,mod3,mod4,mod5) %>% plot
