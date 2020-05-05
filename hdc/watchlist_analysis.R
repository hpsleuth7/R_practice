watchlist_agg <- function(watchlist=NULL,boro=FALSE) {
# Usage: takes dataframe of landlord watchlist, optional borough flag
#        returns new dataframe aggregated by landlord with sum and avg of viols+bldgs

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

