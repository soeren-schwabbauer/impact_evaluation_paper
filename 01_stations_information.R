rm(list = ls())
# create file with stationinformation


#load libraries
library(dplyr)
library(stringr)


# INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/csv_raw/"
OUTPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/.rda/"

if (!file.exists(paste0(INPUT, "stations_information.csv"))){
url <- "https://www.env-it.de/stationen/public/download.do?event=euMetaStation"
download.file(url, destfile = paste0(INPUT, "stations_information.csv"))
}

stations_information <- read.csv(paste0(INPUT, "stations_information.csv"), header = T, sep = ";", skip = 1)

# filter for stations in Hamburg

stations_information <- stations_information %>% 
  
  filter(str_detect(station_code, "HH")) %>%
  
  mutate(station_name = gsub("<df>", "ß", station_name),
         station_name = gsub("<d6>", "Ö", station_name),
         station_name = gsub("<f6>", "ö", station_name),
         station_name = gsub("<fc>", "ü", station_name)) %>%
  
  mutate(station_name = gsub("HH ", "", station_name),
         station_name = gsub("Hamburg ", "", station_name)) %>%
  
  select(station_code, station_name, type_of_station, station_type_of_area)



# save file
save(stations_information, file = paste0(OUTPUT, "stations_information.rda"))
