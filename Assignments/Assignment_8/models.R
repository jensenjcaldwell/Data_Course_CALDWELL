options(scipen = 100)
library(easystats)
library(tidyverse)
library(modelr)


sim3 %>% 
  ggplot(aes(x=x1,
             y=y,
             color=x2))+
  geom_point()+
  geom_smooth(method = "lm")

#Effect of x1 depends on value of x3
#thus they interact

mod1 <- glm(data = sim3,
            formula = y~ x1)
mod2 <- glm(data = sim3,
            formula = y~x1*x2) # y ~ x1 + x2 + x1:x2
mod3 <- glm(data = sim3,
            formula = y ~ x1 + x2)

compare_performance(mod1,mod2,mod3,rank = TRUE)

?compare_performance

add_predictions(sim3,mod2) %>% 
  ggplot(aes(x=x1,y=pred))+
  geom_point()

gather_predictions(sim3,mod1,mod2,mod3) %>% 
  ggplot(aes(x=x1,y=pred,color=x2))+
  geom_point()+
  geom_line()+
  facet_wrap(~model)








mpg %>% glimpse()
mod4 <- glm(data = mpg,
            formula = cty ~ cyl * displ)
add_predictions(mpg,mod4) %>% 
ggplot(aes(x=displ,
           y=pred,
           color=factor(cyl)))+
  geom_point()+
scale_color_viridis_d()


newcars <- data.frame(disp = c(40,4.5,3), cyl = c(4,6,8))

add_predictions(newcars,mod4)


mpg %>% 
  ggplot(aes(x=displ, y=cty))+
  geom_point()+
  geom_smooth(method = "lm", formula = y ~ poly(x,2))

mod5 <- glm(data = mpg,
    formula = cty ~ cyl * poly(displ,2)) 

add_predictions(mpg,mod5) %>% 
  ggplot(aes(x=displ,y=pred,color=cyl))+
  geom_point()


ggplot(mpg, aes(x=log10(cty)))+
  geom_density()


ggplot(mpg, aes(x=displ,y=log(cty)))+
  geom_point()

#####



lehi <- read_rds("Data/Lehi_Home_Sales.RDS")

lehi$net_sold_price

lehi %>% ggplot(aes(x=log10(net_sold_price)))+  #not normally distributed
  geom_density()


library(palmerpenguins)

df <- penguins %>% 
  mutate(female = case_when(sex == "female" ~ TRUE,
                            TRUE ~ FALSE))

df

#logistic regression

mod6 <- glm(data=df,formula = female ~ body_mass_g + bill_length_mm + species, family = "binomial")

predict(mod6,data.frame(body_mass_g=3000,
        bill_length_mm=20,
        species="Gentoo"),
        type = "response")

add_predictions(df,mod6, type = "response") %>% view



mpg %>% mutate(good_mileage = case_when(cty > 25 ~ TRUE,
                                        TRUE ~ FALSE)) %>% 
  glm(data = ., 
      formula = good_mileage ~ cyl *trans*displ,
      family = "binomial") %>% 
  add_predictions(data=mpg,model = ., type = "response") %>% 
  ggplot(aes(x=pred,y=cty))+
  geom_point()
