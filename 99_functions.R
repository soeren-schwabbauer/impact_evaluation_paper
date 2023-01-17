# functions

# overall theme
labs_all <- labs(subtitle = paste0("from ", min(df$year), " to ", max(df$year)), caption = "source: Umweltbundesamt")

# give
availability_stations <- function(station){
df %>% 
  
  filter(station_name == {{station}}) %>%
  
  ggplot(aes(x = year_month, y = type)) +
  geom_tile(colour="white", fill = "grey", size=0.2) +
  
  geom_vline(xintercept = as.Date("2018-02-27"), color = "red", linetype = "dotted") +
  annotate("text", x = as.Date("2017-08-31"), y = 2.5, label = "policy was annonced (27.02.2018)", angle = 90) +
  
  geom_vline(xintercept = as.Date("2018-05-31"), color = "red") +
  annotate("text", x = as.Date("2018-12-30"), y = 2.5, label = "policy was in place (31.05.2018)", angle = 90) +
  
  labs(x = "", y = "", title = paste0("available data for ", station)) +
    labs_all +
  theme_minimal()
}




availability_pollution <- function(type){
  
  df %>% 
  
  filter(type == {{type}}) %>%
  
  ggplot(aes(x = year_month, y = station_name)) +
  geom_tile(colour="white", fill = "grey", size=0.2) +
  
  geom_vline(xintercept = as.Date("2018-02-27"), color = "red", linetype = "dotted") +
#  annotate("text", x = as.Date("2017-08-31"), y = 2.5, label = "policy was annonced (27.02.2018)", angle = 90) +
  
  geom_vline(xintercept = as.Date("2018-05-31"), color = "red") +
#  annotate("text", x = as.Date("2018-12-30"), y = 2.5, label = "policy was in place (31.05.2018)", angle = 90) +
  
  labs(x = "", y = "", title = paste0("available data for ", type),
      subtitle = "for seleted stations") +
  theme_minimal()

}



development_type <- function(type){
  
  df %>% filter(type == {{type}}) %>%
    
    ggplot(aes(x = year_month, y = mean, color = type_of_station)) +
    geom_point() +
    geom_smooth(se = FALSE) +
    
    geom_vline(xintercept = as.Date("2018-05-31"), color = "red") +
    #annotate("text", x = as.Date("2018-06-30"), y = 33, label = "policy was in place (31.05.2018)", angle = 90) +
    
    labs(x = "", y = "", 
         title = paste0("over time development for ", type), 
         subtitle = paste0("of selected station in Hamburg from ", min(df$year), " to ", max(df$year)), 
         color ='name of station', ) +
    
    theme_minimal()
}

