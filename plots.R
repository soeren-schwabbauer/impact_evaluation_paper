rm(list = ls())
# purpose: first plots


#load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)


# INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/"
OUTPUT = "G:/Shared files/impact_evaluation_paper/plots/"

# alle_stationen: alle zusammengefügte stationen, direkt aus den heruntergeladenen files.
# downloadlink: https://www.overleaf.com/download/project/639c706417860e239c540d48/build/1852682647a-a7e5850ab1f632e2/output/output.pdf?compileGroup=standard&clsiserverid=clsi-pre-emp-e2-c-f-f4s9&enable_pdf_caching=true&popupDownload=true

# alle_stationen_editable: zeilennamen auf englisch übersetzt und eine leere spalte gelöscht.

# load file
df <- read.csv2(paste0(INPUT, "alle_stationen_editable.csv"))
# first plots

test %>% filter(state == "Hamburg") %>%
  distinct(station_name)
# 74 teststationen in Hamburg

stattons <- test %>% distinct(station_) %>% pull()

# fahrverbot ab 1.06.2018 in Hamburg Max-Brauer-Allee II (Straße)
max_brauer <- test %>% filter(station_name == "Hamburg Max-Brauer-Allee II (Straße)" & 
                                station_surrounding == "städtisches Gebiet") %>%
  filter(year >= 2017 & year <= 2019) %>%
  ggplot() +
  geom_boxplot(aes(x = date, y = value, group = month_year))

rest <- test %>% filter(state == "Hamburg" &
                          station_name != "Hamburg Max-Brauer-Allee II (Straße)" & 
                          station_surrounding == "städtisches Gebiet") %>%
  filter(year >= 2017 & year <= 2019) %>%
  ggplot() +
  geom_boxplot(aes(x = date, y = value, group = month_year))

grid.arrange(max_brauer, rest, nrow = 2)
# fahrverbot ab 1.06.2018 in Hamburg Stresemannstraße
# Stresemannstraße an Autobahn -> Vergleichbar mit Hamburg habitstraße
streseman <- test %>% filter(station_name == "Hamburg Max-Brauer-Allee II (Straße)" & 
                               station_surrounding == "städtisches Gebiet") %>%
  filter(year >= 2017 & year <= 2019) %>%
  ggplot() +
  geom_boxplot(aes(x = date, y = value, group = month_year))

habicht <- test %>% filter(station_name == "Hamburg Max-Brauer-Allee II (Straße)" & 
                             station_surrounding == "städtisches Gebiet") %>%
  filter(year >= 2017 & year <= 2019) %>%
  ggplot() +
  geom_boxplot(aes(x = date, y = value, group = month_year))

grid.arrange(max_brauer, rest, nrow = 2)


test2 <- test %>% filter(state == "Baden-Württemberg")

test %>%  distinct(station_code) %>% nrow()
