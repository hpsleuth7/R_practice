# ggplot2 practice -- August 2020 #
# following https://github.com/jennybc/ggplot2-tutorial #

library(tidyverse)
library(gapminder)

p <- ggplot(gapminder,aes(x=gdpPercap,y=lifeExp)) #create base plot with axes

p + geom_point()    #test out plotting
p+geom_point()+scale_x_log10()

p <- p+scale_x_log10()  #keep the scale that worked well!

p+aes(color=continent)  #color by continent
print(p+geom_point()+aes(color=continent))     #print method works with ggplot objects

p+geom_point(alpha=1/3,size=3)+aes(color=continent)  #try out transparency and size

p+geom_point()+geom_smooth(lwd=3,se=FALSE) #try out smoothing function

p+geom_point()+geom_smooth(lwd=3,se=FALSE)+facet_wrap(~continent) #facet by continent

ggplot(gapminder)+   #plot year and lifeExp with help of randomness
  aes(x=year,y=lifeExp)+
  geom_jitter(alpha=1/3,size=3)+
  aes(color=continent)

ggplot(gapminder,aes(x=year,y=lifeExp,color=continent))+  # plot same w continent facets
  geom_jitter(alpha=1/3,size=3)+
  facet_wrap(~continent,scales = "free_x")+     #scales allow the facet scales
  scale_color_manual(values=continent_colors)   # to vary from one another by dim


# line plot showing each country's lifeExp over time, by continent
gapminder %>% 
  filter(continent!="Oceania") %>% 
  ggplot()+
  aes(x=year,y=lifeExp,group=country,color=country)+
  geom_line(lwd=1, show.legend=FALSE)+
  facet_wrap(.~continent)+
  scale_color_manual(values=country_colors)+
  theme_bw()+
  theme(strip.text=element_text(size=rel(1.1)))

#dot plot showing lifeExp over time with a trendline, for each continent
gapminder %>% 
  filter(continent!="Oceania") %>% 
  ggplot()+
  aes(x=year,y=lifeExp,color=continent)+
  geom_point(alpha=1/3,size=2,position="jitter")+
  geom_smooth(lwd=2,se=FALSE)+
  facet_wrap(.~continent)+
  scale_color_manual(values = continent_colors) #this seems to just make it darker

#line plot of lifeExp over time for Zimbabwe
gapminder %>% 
  filter(country=="Zimbabwe") %>% 
  ggplot()+
  aes(x=year,y=lifeExp)+
  geom_point()+
  geom_line()

# line plot of lifeExp for several countries
gapminder$country <- fct_reorder2(         #reorder country factors for legend
    gapminder$country,gapminder$year,gapminder$lifeExp)
gapminder %>% 
  filter(country %in% c("Zimbabwe","Cambodia","Canada","Japan")) %>% 
  ggplot()+
  aes(x=year,y=lifeExp,group=country,color=country)+
  geom_point()+
  geom_line()


#strip plot of lifeExp for each continent, with median added in blue
gapminder %>% 
  filter(year>1995) %>% 
  ggplot()+
  aes(x=continent,y=lifeExp)+
  geom_jitter(position=position_jitter(width=0.1,height=0),
              alpha=1/2,color="purple")+
  stat_summary(fun="median",color="blue",geom="point",size=4)

#histogram of all lifeExp values
ggplot(gapminder)+
  aes(x=lifeExp)+
  geom_histogram(binwidth = 1)

ggplot(gapminder)+    #make a weird overlaid histogram -- this plots multiple graphs!
  aes(x=lifeExp,fill=continent)+     # a different approach to faceting
  geom_histogram(binwidth = 1,position="identity")

ggplot(gapminder)+    #frequency polygon = connecting bin midpoints in histogram
  aes(x=lifeExp,color=continent)+     #good for overlaying!
  geom_freqpoly(binwidth = 3)

gapminder %>% 
  filter(continent!="Oceania") %>% 
  ggplot()+  # density plot = probability density function for variable =  
    aes(x=lifeExp,fill=continent)+   # histogram with area under curve = 1!
    geom_density(adjust=1,alpha=.25)

ggplot(gapminder)+
  aes(x=year,y=lifeExp,group=year)+
  geom_boxplot()
