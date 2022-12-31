rm(list = ls())
# purpose: create .rda file


#load libraries
library(dplyr)
library(data.table)
library(purrr)
library(naniar)

# INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/csv_raw/HH_1990-2022/"
OUTPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/.rda/"

load(paste0(OUTPUT, "stations_information.rda"))

# invertiert: alle stationen nebeneinander
# nicht invertiert: alle stationen untereinander

# type of data: Informationen zu gelieferter zeitlicher Auflösung
    # S: "durch das Bundesland mitgelieferte Studnenwerte
    # H: "Halbstundenwerte"
    # T: "Tagesmittelwerte"

# für excel: 
    # Tausendertrennzeihen - Leerzeichen
    # Dezimaltrennzeichen  - Punkt

# Werte für:
  # SCO - Kohlenmonoxid       - inv8 (?)  - SMW - Stundenmittelwert
  # SNO2 - Stickstoffdioxid   - inv1 (?)  - SMW - Stundenmittelwert
  # HCO - Kohlenmonoxid
  # SMP1 - PM10 (Feinstaub)   -           - TMW - Tagesmittelwert
  # SPM2 - PM2_5 ()           -           - TMW - Tagesmittelwert
  # SSO2 - Schwefeldioxid     -           - TMW - Tagesmittelwert


# --> Jedes File hat einzelnen Messtype für ein Jahr

setwd(INPUT)
######  Tagesmittelwerte (für PM10, PM2_5, Schwefel)
TMWs <- list.files(path = INPUT, pattern = "TMW_20221230.csv") %>% map_df(~fread(.)) %>%
  # nur die selecten, die auch in den anderen beiden Vorkommen
  select(Station, Komponente, Datum, TMW) %>%
  replace_with_na(replace = list(`TMW` = -999)) 


######   8h Stundenmittelwerte
SMW8s <- list.files(path = INPUT, pattern = "inv8SMW_20221230.csv") %>% map_df(~fread(.)) %>%
  
  #replace -999 with na
  replace_with_na(replace = list(`8SMW` = -999)) %>%
  
  # mittelwert bilden
  group_by(Station, Komponente, Datum) %>%
  summarise(TMW = mean(`8SMW`, na.rm = TRUE))


######   1h Stundenmittelwerte
SMW1s <- list.files(path = INPUT, pattern = "inv1SMW_20221230.csv") %>% map_df(~fread(.)) %>%
  
  #replace -999 with na
  replace_with_na(replace = list(Wert = -999)) %>%
  
  # mittelwert bilden
  group_by(Station, Komponente, Datum) %>%
  summarise(TMW = mean(Wert, na.rm = TRUE))


df <- bind_rows(TMWs, SMW1s, SMW8s)


# edits am main df

pollution_all <- df %>% 
  
  # rename stations
  rename(station_code = Station,
         type = Komponente,
         date = Datum,
         daily_mean = TMW) %>%
  
  # remove '
  mutate(across(everything(),~ gsub("'", "", .)),
         daily_mean = as.numeric(daily_mean)) %>%
  
  mutate(type = case_when(type == "Schwefeldioxid" ~ "sulfur dioxide",
                          type == "Kohlenmonoxid" ~ "carbon monoxide",
                          type == "Stickstoffdioxid" ~ "nitrogen dioxide",
                          type == "PM10" ~ "PM10",
                          type == "PM2_5" ~ "PM2_5")) %>%
  
  #select year, month, day
  mutate(year = as.numeric(substr(date, 1, 4)),
         month = as.numeric(substr(date, 5,6)),
         day = as.numeric(substr(date, 7,8))) %>%
  
  # match station information
  left_join(stations_information, by = "station_code")




# save file as .rda
save(pollution_all, file = paste0(OUTPUT, "pollution_all.rda"))

