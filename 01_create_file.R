rm(list = ls())
# purpose: create .rda file


#load libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)


# INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/"
OUTPUT = "G:/Shared files/impact_evaluation_paper/"

# alle_stationen: alle zusammengefügte stationen, direkt aus den heruntergeladenen files.
# downloadlink: https://www.overleaf.com/download/project/639c706417860e239c540d48/build/1852682647a-a7e5850ab1f632e2/output/output.pdf?compileGroup=standard&clsiserverid=clsi-pre-emp-e2-c-f-f4s9&enable_pdf_caching=true&popupDownload=true

# alle_stationen_editable: zeilennamen auf englisch übersetzt und eine leere spalte gelöscht.

# load file
df <- read.csv2(paste0(INPUT, "alle_stationen_editable.csv"))


# edits with file
df <- df %>% 
  #set date
  mutate(date = dmy(date),
         month = month(date),
         year = year(date),
         month_year = paste0(month, "_", year), 
                      
         #value as numeric
         value = as.numeric(value))

# save file as .rda
save(df, file = paste0(OUTPUT, "pollution.rda"))

