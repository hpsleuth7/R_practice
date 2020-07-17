# Practice script for tidying data
# From exercises https://github.com/jennybc/lotr-tidy#readme

library(tidyverse)

#read data
fship <- read_csv("data/The_Fellowship_Of_The_Ring.csv")
ttow <- read_csv("data/The_Two_Towers.csv")
retk <- read_csv("data/The_Return_Of_The_King.csv")

lotr_words <- bind_rows(fship,ttow,retk)  #combine into one dat

lotr_words <- lotr_words %>%    #gather wordcount into column by gender
  gather(key="Gender",value="Words",Female,Male)