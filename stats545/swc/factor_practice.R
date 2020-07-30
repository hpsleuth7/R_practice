# STAT 545 -- practice for factors
# https://stat545.com/factors-boss.html

library(tidyverse)
library(gapminder)

fct_count(gapminder$continent)

#filter to wine countries
wine_countries <- c("France","Portugal","Germany","Italy","Spain","South Africa")
wine_dat <- gapminder %>% 
  filter(country %in% wine_countries)

levels(wine_dat$country) #note all factors still there!

levels(droplevels(wine_dat)$country) #droplevels() works on dataframe or vector

#dropping levels through fct_drop
test <- 
  wine_dat %>% 
  mutate(country=fct_drop(country),continent=fct_drop(continent))
levels(test$continent)

#dropping levels for all factor cols through mutate across
test <- 
  wine_dat %>% 
  mutate(across(where(is.factor),fct_drop))
levels(test$country)

### Practice re-ordering factors ###

gapminder$continent %>%  # continent by order of frequency
  fct_infreq() %>% 
  levels()

fct_reorder(gapminder$country,gapminder$lifeExp,min) %>%  # country by min lifeExp
  levels()

### Recode levels ###

some_countries <- gapminder %>% 
  filter(country %in% c("Australia","Korea, Dem. Rep.","Korea, Rep.")) %>% 
  droplevels()

some_countries$country <- fct_recode(some_countries$country,
    "Oz"="Australia","South Korea"= "Korea, Dem. Rep.","North Korea"="Korea, Rep.")



