library(tidyverse)
library(gapminder)

my_gap <- gapminder

my_gap %>% 
  group_by(continent) %>% 
  summarize(avg_life = mean(lifeExp)

