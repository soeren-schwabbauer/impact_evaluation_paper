rm(list = ls())

#load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(janitor)


#Time series data with the plotly package
install.packages("plotly")
library(plotly)
#Plot with TSstudio package
install.packages("TSstudio")
library(TSstudio)

#Test assumption: Did enough vehicles pass the Stresemannstraße and the Max-Brauer-Allee?
#Have there been construction sites that migitated the traffic?

#1.) Max-Brauer Allee: DTV 2004 - 2020 for normal vehicles
#link: https://drive.google.com/drive/folders/0AC0Gn6dXRbWLUk9PVA

#DTV = average numbe of passing vehicles per day in a certain year
#DTVw = average number of passing vehicles per work day in a certain year
#SV Anteil = heavy vehicle in percentage points
#Construction = have there been construction sites? 


#load file
v1 <- read.csv2("Verkehrsstaerke_Hamburg_2004_2020.csv")
View(v1)


#Row to names
v1 <- row_to_names(v1, row_number = 3, remove_rows_above = TRUE, remove_row = TRUE)
View(v1)

#----------------------------------------------------------
#Stationen Max-Breuer Allee: 2687, 2682, 2683, 2686, 2868


#Station 2682: Row 561 and row 4 for years
v2682 <- v1[c (4, 561),]
View(v2682)

#Remove cols
v2682 <- v2682[-c(1:4)]

#Rows to cols
v_2682_new <- as.data.frame(t(v2682))
View(v_2682_new)

#Rename cols
colnames(v_2682_new)[colnames(v_2682_new) == "4"] <- "Year"
colnames(v_2682_new)[colnames(v_2682_new) == "561"] <- "Average_cars_2682"

#Change year column to time format
v_2682_new$Year <- as.integer(v_2682_new$Year)
v_2682_new$Year <- as.Date(paste(v_2682_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2682_new$Average_cars_2682 <- as.integer(v_2682_new$Average_cars_2682)

#TS plot
ts_plot(v_2682_new, title = "Traffic volume Station 2682",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")

#--------------------------------------------------

#Station 2683: 	Max-Brauer-Allee NO Holstenstraße

#Station 2683: Row 565 and row 4 for years
v2683 <- v1[c (4, 565),]
View(v2683)

#Remove cols
v2683 <- v2683[-c(1:4)]

#Rows to cols
v_2683_new <- as.data.frame(t(v2683))
View(v_2683_new)

#Rename cols
colnames(v_2683_new)[colnames(v_2683_new) == "4"] <- "Year"
colnames(v_2683_new)[colnames(v_2683_new) == "565"] <- "Average_cars_2683"

#Change year column to time format
v_2683_new$Year <- as.integer(v_2683_new$Year)
v_2683_new$Year <- as.Date(paste(v_2683_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2683_new$Average_cars <- as.integer(v_2683_new$Average_cars)

#TS plot
ts_plot(v_2683_new, title = "Traffic volume Station 2683",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")


#Merge with 2682
merge <- merge(v_2682_new, v_2683_new, by.x = "Year", by.y = "Year")
View(merge)
merge <-subset(merge, select = -c(4))
str(merge)
merge$Average_cars_2683 <- as.integer(merge$Average_cars_2683)

#Plot the two in one time series
ts_plot(merge, title = "Traffic volume, station 2682 and 2683, M+ax-Brauer-Allee", 
        Xtitle = "Time, 2004 - 2020", 
        Ytitle = "Average number of cars per day")

#--------------------

#Station 2686: 	Holstenstraße S Max-Brauer-Allee

#Station 2686: Row 569 and row 4 for years
v2686 <- v1[c (4, 569),]
View(v2686)

#Remove cols
v2686 <- v2686[-c(1:4)]

#Rows to cols
v_2686_new <- as.data.frame(t(v2686))
View(v_2686_new)

#Rename cols
colnames(v_2686_new)[colnames(v_2686_new) == "4"] <- "Year"
colnames(v_2686_new)[colnames(v_2686_new) == "569"] <- "Average_cars_2686"

#Change year column to time format
v_2686_new$Year <- as.integer(v_2686_new$Year)
v_2686_new$Year <- as.Date(paste(v_2686_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2686_new$Average_cars <- as.integer(v_2686_new$Average_cars)

#TS plot
ts_plot(v_2686_new, title = "Traffic volume Station 2686",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")


#Merge with 2683 and 2682
merge_1 <- merge(merge, v_2686_new, by.x = "Year", by.y = "Year", all = TRUE)
View(merge_1)
merge_1 <-subset(merge_1, select = -c(5))
str(merge_1)
merge_1$Average_cars_2686 <- as.integer(merge_1$Average_cars_2686)


#Plot the three in one time series
ts_plot(merge_1, title = "Traffic volume, station 2682,2683 and 2686, M+ax-Brauer-Allee", 
        Xtitle = "Time, 2004 - 2020", 
        Ytitle = "Average number of cars per day")

#-----------------------------------------------------------

#Station 2687: Max-Brauer-Allee SW Holstenstraß

#Station 2687: Row 573 and row 4 for years
v2687 <- v1[c (4, 573),]
View(v2687)

#Remove cols
v2687 <- v2687[-c(1:4)]

#Rows to cols
v_2687_new <- as.data.frame(t(v2687))
View(v_2687_new)

#Rename cols
colnames(v_2687_new)[colnames(v_2687_new) == "4"] <- "Year"
colnames(v_2687_new)[colnames(v_2687_new) == "573"] <- "Average_cars_2687"

#Change year column to time format
v_2687_new$Year <- as.integer(v_2687_new$Year)
v_2687_new$Year <- as.Date(paste(v_2687_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2687_new$Average_cars <- as.integer(v_2687_new$Average_cars)

#TS plot
ts_plot(v_2687_new, title = "Traffic volume Station 2687",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")

#Merge with 2683, 2682, 2686
merge_2 <- merge(merge_1, v_2687_new, by.x = "Year", by.y = "Year", all = TRUE)
View(merge_2)
merge_2 <-subset(merge_2, select = -c(6))
str(merge_2)
merge_2$Average_cars_2687 <- as.integer(merge_2$Average_cars_2687)


#Plot the three in one time series
ts_plot(merge_2, title = "Traffic volume, station 2682,2683, 2686, 2687, Max-Brauer-Allee", 
        Xtitle = "Time, 2004 - 2020", 
        Ytitle = "Average number of cars per day")

#.-----------------------------

#Station 2868: Platz der Republik W Max-Brauer-Allee

#Station 2868: Row 681 and row 4 for years
v2868 <- v1[c (4, 681),]
View(v2868)

#Remove cols
v2868 <- v2868[-c(1:4)]

#Rows to cols
v_2868_new <- as.data.frame(t(v2868))
View(v_2868_new)

#Rename cols
colnames(v_2868_new)[colnames(v_2868_new) == "4"] <- "Year"
colnames(v_2868_new)[colnames(v_2868_new) == "681"] <- "Average_cars_2868"

#Change year column to time format
v_2868_new$Year <- as.integer(v_2868_new$Year)
v_2868_new$Year <- as.Date(paste(v_2868_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2868_new$Average_cars <- as.integer(v_2868_new$Average_cars)

#TS plot
ts_plot(v_2868_new, title = "Traffic volume Station 2868",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")

#Merge with 2683, 2682, 2686
merge_3 <- merge(merge_2, v_2868_new, by.x = "Year", by.y = "Year", all = TRUE)
View(merge_3)
merge_3 <-subset(merge_3, select = -c(7))
str(merge_3)
merge_3$Average_cars_2868 <- as.integer(merge_3$Average_cars_2868)


#Plot the five in one time series
ts_plot(merge_3, title = "Traffic volume, station 2682,2683, 2686, 2687, Max-Brauer-Allee", 
        Xtitle = "Time, 2004 - 2020", 
        Ytitle = "Average number of cars per day")

#.-----------------------------