rm(list = ls())

# purpose: describe procedure of data analyisis and findings!

library(dplyr)
library(ggplot2)
library(tidyr)

# INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/.rda/"
OUTPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/plots/"


load(paste0(INPUT, "pollution_all.rda"))


df <- pollution_all %>% 
  
  # calculate monthly mean
  group_by(station_code, type, year_month, year, month, station_name, type_of_station, station_type_of_area) %>%
  summarise(mean = mean(daily_mean, na.rm = TRUE)) %>%
  ungroup()


# load sheet with functions
source("99_functions.R")

##### availability of pollution for Max-Brauer-Allee (Straße) ###########################

availability_stations("Max-Brauer-Allee (Straße)")

max_brauer_na <- df %>% filter(station_name == "Max-Brauer-Allee (Straße)") %>% filter_all(any_vars(is.na(.)))
# PM10 record started in 2000; however no sufficient data until 2003


availability_stations("Stresemannstraße")
# note: PM10 record started in 2000

# -> filter out all data before 2000, since no record of PM10 before
df <- df %>% filter(year >= 2000)



##### check PM10 availability of other stations within Hamburg #################

# get an overview at which point in time which station was measuring PM10
df %>% filter(type == "PM10") %>%
  
  ggplot(aes(x = year_month, y = station_name)) +
  geom_tile(color = "grey", size = 0.25) +
  
  geom_vline(xintercept = as.Date("2011-01-01"), color = "blue") +
  geom_vline(xintercept = as.Date("2015-01-01"), color = "blue") +
  geom_vline(xintercept = as.Date("2004-01-01"), color = "blue") +
  
  geom_vline(xintercept = as.Date("2018-05-31"), color = "red") +
  annotate("text", x = as.Date("2018-12-30"), y = 9, label = "policy was in place (31.05.2018)", angle = 90) +
  
  labs(x="", y="", title = "Availability of PM10 for stations in Hamburg") +
  labs_all +
  theme_minimal(base_size = 12)

# Check precisely, what is possible:
# Stresemannstraße: ab 2000-01

# Wilhelmsburg: ab 2000-08-01; nas: 2007-12 bis 2008-02

# Veddel: ab 2001-01-01

# Sternschanze: ab 2000-09

# Hafen: ab 2015-10

# Habichtstaße: ab 2004-02

# Flughafen Nord: ab 2002-05 bis 2019-12

# Finkenwerder west: ab 2004-02 bis 2019 - 12

# Billbrook: ab 2000-07; nas bei 2008-04 und 2008-05

# Altona Elbhang: ab 2011-12

# note: to include as many stations as possible: start in 2015 (unitl 2022)
# -> 6 stations for comparison

# note: only up to the end of 2019 
# -> 8 stations for comparison


# -> choose final stations
final_stations <- c("Stresemannstraße", "Max-Brauer-Allee (Straße)", "Wilhelmsburg" , "Veddel", "Sternschanze", "Hafen", "Habichtstraße", "Flughafen Nord", "Finkenwerder West", "Billbrook", "Altona Elbhang")

#-> Idea: Start in 2015-08 till 2019-12
final_months <- seq.Date(as.Date("2015-10-01"), as.Date("2019-12-01"), by = "month")

df <- df %>% filter(year_month %in% final_months) %>%
  filter(station_name %in% final_stations)


##### check availabilty of other pollution types for the selected stations #####

availability_pollution("nitrogen dioxide")
# note: perect coverage for nitrogen dioxide for all stations

availability_pollution("PM2_5")
# note: only Wilhelmsburg, Veddel, Sternschanze & Habichtstraße (-> use as additial predictor)

availability_pollution("sulfur dioxide")
# note: coverage for Wilhelmsburg, Veddel, Sternschanze, Hafen, Billbrook, Altona Elbhang

availability_pollution("carbon monoxide")
# note: coverage only for Habichtstraße & Flughafen Nord

# -> choose final pollution values
final_pollution <- c("PM10", "nitrogen dioxide") 


##### development over time
development_type("PM10")

development_type("nitrogen dioxide")



###### apply filters 
df <- df %>% filter(year_month %in% final_months) %>%
  filter(type %in% final_pollution) %>%
  filter(station_name %in% final_stations)





##### add numeric ids to stations & times, since synth does not work otherwise
df <- df %>%
  
  group_by(year_month) %>%
  mutate(date_id = cur_group_id(),
         date_id = as.numeric(date_id)) %>%
  ungroup() %>%
  
  group_by(station_name) %>%
  mutate(station_id = cur_group_id(),
         station_id = as.numeric(station_id)) 

# transfer to wider df; to apply synth
df_wide <- df %>% pivot_wider(names_from = c("type"), values_from = "mean")

# save the selected df
save(df, file = paste0(INPUT, "pollution_all_synth.rda"))
save(df_wide, file = paste0(INPUT, "pollution_all_synth_wide.rda"))

