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

ggplot(dat)+          #admit rate against 4-year grad, color by % Black
  aes(x=adm_rate,y=c150_4)+
  geom_point()+
  geom_smooth(se=FALSE)+
  aes(color=ugds_black)

#plot top ten admit rates of schools > 20,000 undergrads
dat$instnm<-fct_reorder(factor(dat$instnm),dat$adm_rate)
dat %>% filter(ugds>20000) %>%  arrange(desc(adm_rate)) %>% head(10) %>% 
  ggplot()+
  aes(x=adm_rate,y=instnm)+
  geom_bar(stat="identity")
