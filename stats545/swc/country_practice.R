library(tidyverse)
library(gapminder)

my_gap <- gapminder

continent_life_exp <-  my_gap %>% 
  group_by(continent) %>% 
  summarize(avg_life = mean(lifeExp))

print(continent_life_exp)

continent_life_gdp <-  my_gap %>% 
  filter(year %in% c(1952,2007)) %>% 
  group_by(continent,year) %>% 
  summarize_at(vars(lifeExp,gdpPercap),list(~median(.),~mean(.)))

print(continent_life_gdp)

asia_extreme_life <-  my_gap %>% 
  filter(continent=='Asia') %>% 
  group_by(year) %>%
  summarize(min_lifeExp=min(lifeExp),max_lifeExp=max(lifeExp))

print(asia_extreme_life)

lifeExp_comparison <- my_gap %>% #Change in lifeExp from 1952 for each country and year
  group_by(country) %>% 
  select(country, year, lifeExp) %>% 
  arrange(year) %>% 
  mutate(lifeExp_growth=lifeExp-first(lifeExp)) %>% 
  filter(year>1963)

print(lifeExp_comparison)

my_gap %>%   #life expectancy extremes in Asia by year
  filter(continent=='Asia') %>% 
  select (year, country, lifeExp) %>% 
  group_by(year) %>% 
  filter(min_rank(desc(lifeExp))<2 | min_rank(lifeExp)<2) %>% 
  arrange(year) %>% 
  print(n=Inf)

my_gap %>% # largest 5-year drop in lifeExp by continent
  arrange(year) %>% 
  group_by(continent, country) %>% 
  # create delta_le column representing change in lifeExp from 
  # previous country record (5 yrs ago)
  mutate(delta_le=lifeExp-lag(lifeExp)) %>%
  select(continent, country, year, delta_le) %>% 
  summarize(worst_le_delta=min(delta_le,na.rm=TRUE)) %>% 
  slice_min(worst_le_delta) %>% 
  print(n=Inf)
    
  


  