# ggplot2 practice -- August 2020 #
# following https://github.com/jennybc/ggplot2-tutorial #

library(tidyverse)
library(gapminder)

p <- ggplot(gapminder,aes(x=gdpPercap,y=lifeExp)) #create base plot with axes

p + geom_point()    #test out plotting
p+geom_point()+scale_x_log10()

p <- p+scale_x_log10()  #keep the scale that worked well!

p <- p+aes(color=continent)  #color by continent
print(p+geom_point())     #print method works with ggplot objects

p+geom_point(alpha=1/3,size=3)  #try out transparency and size

p+geom_point()+geom_smooth(lwd=3,se=FALSE)