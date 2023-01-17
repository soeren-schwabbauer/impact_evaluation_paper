rm(list = ls())

#load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(janitor)
library(rio)
library(ggplot2)

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


# INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/.rda/"
OUTPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/plots/"


#download file
if (!file.exists(paste0(INPUT, "traffic_information.rda"))){
  url <- "https://www.hamburg.de/contentblob/7512282/6cfa192436efa7bc62762ebc474c7893/data/dtv-dtvw-2004-2020-download.xlsx"
  df <- rio::import(url)
  save(df, paste0(INPUT, "traffic_information.rda"))
}else {
  load(paste0(INPUT, "traffic_information.rda"))
}


df <- df %>% 
  
  # ersten 3 reihen leer -> filtern
  filter(!row_number() %in% c(1:3)) %>%
  row_to_names(1) %>%
  
  # DTW für alle Tage (inkl. Wochenende)
  filter(Kategorie == "DTV (Kfz/24h)") %>% select(-Kategorie) %>%
  
  # format into long table
  pivot_longer(names_to = "year", cols = as.character(c(2004:2020))) %>% 
  
  # numeric, um damit zu rechnen
  mutate(value = as.numeric(value),
         year = as.numeric(year))


# 2687 ist fahrverbot auf max-brauer-straße
# 2686 ist ausgewiesene alternativroute

# 2683 ist vor fahrverbotsstück


df %>% 
  
  filter(Zählstelle %in% c("2687", "2686", "2683")) %>%
  
  ggplot(aes(y = value, x = year)) +
  geom_line(aes(colour = Zählstelle))
  


#----------------------------------------------------------




#Stationen Max-Breuer Allee: 2687, 2682, 2683, 2686, 2868


#Station 2682: Row 561 and row 4 for years
v2682 <- df[c (4, 561),]
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
        Ytitle = "Average number of cars per day", 
        slider = TRUE)

#.-----------------------------


#2.) Stresemannstraße: In the Stresemannstraße, the ban only applies to lorry  trucks with 
#a weight higher than 3,5 t 
#Thus, we look at the DTV for Schwerverkehr
#SV-Anteil am DTVw (%) 

#----------------------------------

#Station 2641: Row 525 - 528 
#Construction influence: 2005 and 2017

#Reduced
# We need DTVw and SV-share 
v_2634 <- v1[c (4, 526, 527),]
View(v_2634)

#Remove cols
v_2634 <- v_2634[-c(1:4)]

#Rows to cols
v_2634_new <- as.data.frame(t(v_2634))
View(v_2634_new)

#Rename cols
colnames(v_2634_new)[colnames(v_2634_new) == "4"] <- "Year"
colnames(v_2634_new)[colnames(v_2634_new) == "526"] <- "DTVw_2634"
colnames(v_2634_new)[colnames(v_2634_new) == "527"] <- "sv"

#Change year column to time format
v_2634_new$Year <- as.integer(v_2634_new$Year)
v_2634_new$Year <- as.Date(paste(v_2634_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2634_new$DTVw_2634 <- as.integer(v_2634_new$DTVw_2634)


#Make to percentage 
v2634 <- v_2634_new
v2634$sv <- ifelse(v2634$sv == 5, 0.05, 0.04)
View(v2634)

#Create new column by multiplying DTVw and sv
v2634$truck <- v2634$DTVw_2634 * v2634$sv
View(v2634)

#Reduce
v2634 <- v2634[-c(2,3)]

#Plot
ts_plot(v2634, title = "Traffic volume Station 2641, Stresemannstraße",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of heavy cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")


#------------------------------------------

#Station 2641: Row 529 - 532
#Construction influence: -

#Reduced
# We need DTVw and SV-share 
v_2641 <- v1[c (4, 530, 531),]
View(v_2641)

#Remove cols
v_2641 <- v_2641[-c(1:4)]

#Rows to cols
v_2641_new <- as.data.frame(t(v_2641))
View(v_2641_new)

#Rename cols
colnames(v_2641_new)[colnames(v_2641_new) == "4"] <- "Year"
colnames(v_2641_new)[colnames(v_2641_new) == "530"] <- "DTVw_2641"
colnames(v_2641_new)[colnames(v_2641_new) == "531"] <- "sv"

#Change year column to time format
v_2641_new$Year <- as.integer(v_2641_new$Year)
v_2641_new$Year <- as.Date(paste(v_2641_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2641_new$DTVw_2641 <- as.integer(v_2641_new$DTVw_2641)
v_2641_new$sv <- as.integer(v_2641_new$sv)

#Make to percentage 
v2641 <- v_2641_new
v2641$sv <- ifelse(v2641$sv == 10, 0.1, v2641$sv)
v2641$sv <- ifelse(v2641$sv == 9, 0.09, v2641$sv)
v2641$sv <- ifelse(v2641$sv == 8, 0.08, v2641$sv)
v2641$sv <- ifelse(v2641$sv == 7, 0.07, v2641$sv)
View(v2641)
str(v2641)


#Create new column by multiplying DTVw and sv
v2641$truck <- v2641$DTVw_2641 * v2641$sv
View(v2641)

#Reduce
v2641 <- v2641[-c(2,3)]

#Plot
ts_plot(v2641, title = "Traffic volume Station 2641, Stresemannstraße",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of heavy cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")


#---------------------------------

#Station 2646: Row 537 - 540
#Construction influence: -

#Reduced
# We need DTVw and SV-share 
v_2646 <- v1[c (4, 538, 539),]
View(v_2646)

#Remove cols
v_2646 <- v_2646[-c(1:4)]

#Rows to cols
v_2646_new <- as.data.frame(t(v_2646))
View(v_2646_new)

#Rename cols
colnames(v_2646_new)[colnames(v_2646_new) == "4"] <- "Year"
colnames(v_2646_new)[colnames(v_2646_new) == "538"] <- "DTVw_2646"
colnames(v_2646_new)[colnames(v_2646_new) == "539"] <- "sv"

#Change year column to time format
v_2646_new$Year <- as.integer(v_2646_new$Year)
v_2646_new$Year <- as.Date(paste(v_2646_new$Year, 12, 31, sep = "-"))

#Change average number of cars to integer
v_2646_new$DTVw_2646 <- as.integer(v_2646_new$DTVw_2646)
v_2646_new$sv <- as.integer(v_2646_new$sv)

#Make to percentage 
v2646 <- v_2646_new
v2646$sv <- ifelse(v2646$sv == 8, 0.08, v2646$sv)
v2646$sv <- ifelse(v2646$sv == 7, 0.07, v2646$sv)
v2646$sv <- ifelse(v2646$sv == 6, 0.06, v2646$sv)
View(v2646)
str(v2646)


#Create new column by multiplying DTVw and sv
v2646$truck <- v2646$DTVw_2646 * v2646$sv
View(v2646)

#Reduce
v2646 <- v2646[-c(2,3)]

#Plot
ts_plot(v2646, title = "Traffic volume Station 2646, Stresemannstraße",
        Xtitle = "Years 2004 - 2020",
        Ytitle = "Average number of heavy cars per day in a certain year", 
        color = "green", 
        width = "3",
        line.mode =  "lines+markers")

#--------------------------------

#All three stations into one plot 
merger <- merge(v2634, v2641, by.x = "Year", by.y = "Year", all = TRUE)
View(merger)
merger_1 <- merge(merger, v2646, by.x = "Year", by.y = "Year", all = TRUE)
View(merger_1)

#Colnames change
colnames(merger_1)[colnames(merger_1) == "truck.x"] <- "sv_2634"
colnames(merger_1)[colnames(merger_1) == "truck.y"] <- "sv_2641"
colnames(merger_1)[colnames(merger_1) == "truck"] <- "sv_2646"

#Plot all three stations into 1 time series
ts_plot(merger_1, title = "Traffic volume heavy vehicles, stations 2634, 2641, 2646, Stresemannstraße", 
        Xtitle = "Time, 2004 - 2020", 
        Ytitle = "Average number of heavy cars per day")

