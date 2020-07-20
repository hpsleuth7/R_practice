#Practice with Input and Output in R 
#  https://stat545.com/import-export.html#import-export
library(tidyverse)
library(gapminder)
library(fs)

#grap a tsv file for practice
(gap_tsv <- path_package("gapminder","extdata","gapminder.tsv")) 

#read in using readr function
gapminder <- read_tsv(gap_tsv)

gapminder <- gapminder %>%  #factor the char columns
  mutate(country=factor(country),continent=factor(continent))

# create table of max life expectancies by country, for export practice
gap_life_exp <- gapminder %>%
  group_by(country, continent) %>% 
  summarise(life_exp = max(lifeExp)) %>% 
  ungroup()

gap_life_exp <- gap_life_exp %>%  #order factors by life_exp, inc
  mutate(country=fct_reorder(country,life_exp))


export to csv
write_csv(gap_life_exp,"max_life_exp.csv")