### get traffic density data 
### get overview of actual impact
### apply gsynth
rm(list = ls())

library(dplyr)
library(ggplot2)
library(rio)
library(janitor)
library(scales)


INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/.rda/"

###### load/download file ######################################################

if(!file.exists(paste0(INPUT, "traffic_hamburg.rda"))){
  
    url <- "https://www.hamburg.de/contentblob/7512282/6cfa192436efa7bc62762ebc474c7893/data/dtv-dtvw-2004-2020-download.xlsx"
    df_traffic <- rio::import(url)
    save(df_traffic, file = paste0(INPUT, "traffic_hamburg.rda"))
} else{

  load(paste0(INPUT, "traffic_hamburg.rda"))
}

##### make df usable ###########################################################

df_traffic <- df_traffic %>%  
  
  # remove header and make row to columnnames
  filter(!row_number() %in% c(1:3)) %>%     
  row_to_names(1) %>%
  
  # rename columns
  rename(station_id = Zählstelle) %>%
  rename(station_name = Bezeichnung) %>%
  rename(category = Kategorie) %>%
  select(-Ebene) %>%
  
  # format from wide table to long table
  pivot_longer(names_to = "year", cols = as.character(c(2004:2020))) %>%
  
  # change stringtype
  mutate(year = as.numeric(year),
         value = str_replace(value, "B", "1"),
         value = as.numeric(value),
         category = str_replace(category, "Baustelleneinfluss", "impact due to construction site"),
         station_id = as.numeric(station_id))  
  
  
  
##### calculate number of big vehicles (Schwerverkehr) #########################

df_traffic <- df_traffic  %>% 
  
  filter(category == c("DTVw (Kfz/24h)", "SV-Anteil am DTVw (%)"))%>%
  group_by(year, station_id, station_name) %>%
  
  summarise(`DTVw (SV/24h)` = prod(value)/100) %>%
  ungroup() %>%
  pivot_longer(names_to = "category", cols = "DTVw (SV/24h)") %>%
  
  bind_rows(df_traffic) 
  

#### attach station information from excel sheet ###############################

traffic_groups <- read_excel(paste0(INPUT, "traffic_groups.xlsx"))

df_traffic <- left_join(df_traffic, traffic_groups, by = "station_id") %>%
  
  # filter out irrelevant stations
  filter(!is.na(type))


##### check for construction site impact #######################################

df_traffic %>% filter(category == "impact due to construction site") %>%
  
  mutate(value = case_when(value == 1 ~ "yes",
                           is.na(value) ~ "no")) %>%
    
  ggplot(aes(x = year, y = type)) +
    
  geom_tile(color = "white", size = 0.25, aes(fill = value))+
#  theme(panel.background=element_rect(fill="grey", colour="grey")) +
  labs(title = "Impact of constrution site on the routes", subtitle = "according to the official dataset", fill = "" ) +
  
  geom_vline(xintercept = 2018+151/365 -0.5, color = "red") +
  geom_vline(xintercept = c(2015- 0.5 , 2020+0.5), color = "blue") +

  
  theme_minimal()

# -> Max-Brauer-Allee alternative route could have been impacted


#### build graph to check for traffic flow #####################################

graph <- function(street){
  
  # different ways to deal with the two treatment streets
  if(street == "Stresemannstraße"){
    df_fun <- df_traffic %>% filter(category == "DTVw (SV/24h)")
    labels <- labs(title = "average daily number of trucks per year",
                   y = "average daily number of trucks") 
  }else{
    df_fun <- df_traffic %>% filter(category == "DTV (Kfz/24h)")
    labels <- labs(title = "average daily total traffic per year",
                   y = "average daily total traffic")
  }
  
  # plot graph
  df_fun %>% filter(str_detect(type, street)) %>%
    
    group_by(year, type) %>%
    summarise(`average number of cars` = mean(value, na.rm = TRUE)) %>%
     
    ggplot(aes(y = `average number of cars`, x = year, color = type)) +
    geom_point() +
    geom_line() +
    geom_vline(xintercept = 2018+151/365, color = "red") +
 #   annotate("text", x = 2018+250/365, y = 0, hjust = 0, label = "policy was in place (31.05.2018)", angle = ) +
    
    scale_y_continuous(labels = comma) +
    labs(x = "", subtitle = paste0("in the ", street)) +
    labels +
    scale_x_continuous(breaks = seq(min(df_traffic$year), max(df_traffic$year), by = 1)) +
    theme_minimal()
    
  }

# excecute function
graph("Stresemannstraße")
# -> too many na's in Stresemann truck data for a sufficient result
graph("Max-Brauer-Allee")


##### run synth method #########################################################

# policy was announced and in place in 2018
# -> control time id: 2019 & 2020

synth_fun <- function(street){
  
  if(street == "Stresemannstraße"){
    df_fun <- df_traffic %>% filter(category == "DTVw (SV/24h)")

  }else{
    df_fun <- df_traffic %>% filter(category == "DTV (Kfz/24h)")

  }
#
  test <- df_traffic %>% filter(category == "DTVw (SV/24h)")
  #
  
test <- test %>%
  
  group_by(type) %>%
  mutate(type_id = cur_group_id(),
         type_id = as.numeric(type_id)) 

id_treat <- filter()

  #apply synth

  
  
}

