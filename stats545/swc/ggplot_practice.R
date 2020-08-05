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

