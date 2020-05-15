### Admit Analysis
### Usage: function reads CSV in local data directory 
###      Prints and returns plot of smoothed curve of DOC admissions in 2020
library("tidyverse")
library("quantreg")

plot_admits <- function() {

  admits <- read.csv("data/Inmate_Admissions_2020-05-10.csv")


  recent <- mutate(admits, admitted_dt= 
		as.Date(substr(levels(admitted_dt)[admitted_dt],1,10),
		format='%m/%d/%Y')) %>%
    filter(admitted_dt >= as.Date("2020/1/1")) %>%
    group_by(admitted_dt) %>%
    summarize(total_admits = n())

  plot <- ggplot(data=recent) +
    aes(x=admitted_dt,y=total_admits) +
    geom_smooth()
    
  print(plot)
  
  return(plot)
  
}



