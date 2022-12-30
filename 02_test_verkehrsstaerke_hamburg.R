rm(list = ls())

#load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(janitor)

#Test assumption: Did enough vehicles pass the Stresemannstraße and the Max-Brauer-Allee?
#Have there been construction sites that migitated the traffic?

#1.) Max-Brauer Allee: DTV 2004 - 2020 for normal vehicles
#link: https://drive.google.com/drive/folders/0AC0Gn6dXRbWLUk9PVA

#DTV = average number of passing vehicles per day in a certain year
#DTVw = average number of passing vehicles per work day in a certain year
#SV Anteil = heavy vehicle in percentage points
#Construction = have there been construction sites? 

#load file
v1 <- read.csv2("Verkehrsstaerke_Hamburg_2004_2020.csv")

#Row to names
v1 <- row_to_names(v1, row_number = 4, remove_rows_above = TRUE, remove_row = TRUE)

#Measuring stations: 2868, 2687 (in ban area), 2682, 2683, 2686



