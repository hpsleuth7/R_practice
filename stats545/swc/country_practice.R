library(tidyverse)
library(gapminder)

my_gap <- gapminder

continent_life_exp = my_gap %>% 
  group_by(continent) %>% 
  summarize(avg_life = mean(lifeExp))

print(continent_life_exp)

continent_life_gdp = my_gap %>% 
  filter(year %in% c(1952,2007)) %>% 
  group_by(continent,year) %>% 
  summarize_at(vars(lifeExp,gdpPercap),list(~median(.),~mean(.)))

print(continent_life_gdp)

asia_extreme_life = my_gap %>% 
  filter(continent=='Asia') %>% 
  group_by(year) %>%
  summarize(min_lifeExp=min(lifeExp),max_lifeExp=max(lifeExp))

print(asia_extreme_life)
  