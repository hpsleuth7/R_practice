watchlist_agg <- function(watchlist=NULL,boro=FALSE) {
  # Usage: takes dataframe of landlord watchlist, optional borough flag
  #	"Brooklyn" / "Queens" / "Manhattan" / "Bronx" / "Staten Island"
  #    returns new dataframe aggregated by landlord with sum and avg of viols+bldgs

  library(dplyr)
  library(stringr)
  ###


  aggregated <- watchlist
  if (class(boro)[1]=="character") {
    aggregated <- filter(aggregated,borough == boro)
    boro<-str_to_upper(boro)
  }   
  aggregated <- mutate(aggregated,landlord_name = str_to_title(landlord)) %>% 
  select(landlord_name, units, violations) %>% 
  group_by(landlord_name) %>% 
  summarize(
    buildings = n(),
    total_units = sum(units),
    avg_bldg_size = mean(units),
    total_viol = sum(violations),
    avg_bldg_viol = mean(violations)
  ) %>% 
  ungroup() %>% 
  arrange(desc(buildings))
  
  return(aggregated)

}

get_top_ten <- function(landlords=NULL) {
  # Usage: takes dataframe of watchlist records aggregated by landlord
  #	returns ordered dataframe of ten landlords with most violations

  library(dplyr)
  ###

  top_ten <- arrange(landlords,desc(total_viol)) %>%
    head(n=10) %>%
    mutate(landlord_name=str_remove(landlord_name,"Properties"),
      landlord_name=forcats::fct_reorder(landlord_name,total_viol))

return(top_ten)

}

plot_landlord_viols <- function(landlords=NULL) {
  # Usage: takes dataframe of landlords with total_viols column
  # Plots column chart, saves as PDF in working directory, and returns plot object
  
  library(tidyverse)
  ###
  
  # set output to write as "violations_chart_<datetime>.pdf"
  fname <- paste("violations_chart_",as.character(Sys.time()),".pdf")
  pdf(fname)
  
  plot <- ggplot(data = landlords) +
    aes(y=total_viol, x = landlord_name) +
    geom_col() +
    coord_flip() +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(margin=margin(r=-15)),
      plot.caption = element_text(face="italic",color="darkgrey",margin=margin(t=10))) +
    labs(
      title = '10 "Worst" Landlords in NYC',
      subtitle = 'Total HPD Violations',
      x=NULL,
      y="Number of Violations",
      caption = "Source: watchlist"
    )
  
  print(plot)
  dev.off()
  
}
  



