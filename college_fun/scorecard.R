# Fun with 2019 college scorecard data

library(tidyverse)

dat <- read_csv('data/MERGED2018_19_PP.csv',col_names = TRUE,
                col_types = 'iiiccciiiilddiiiiiiiddi-ddddddddddiidddddddd')

# plot admit rate against 4-year grad rate, color for HBCU
ggplot(dat)+
  aes(x=adm_rate,y=c150_4)+
  geom_point()+
  geom_smooth(se=FALSE)+
  aes(color=hbcu)

ggplot(dat)+
  aes(x=adm_rate,y=c150_4)+
  geom_point()+
  geom_smooth(se=FALSE)+
  aes(color=ugds_black)