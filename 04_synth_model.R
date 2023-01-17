rm(list = ls())

# purpose: apply synth to Max-Brauer-Allee (Straße)

library(dplyr)
library(gridExtra)
library(Synth)

# INPUT, OUTPUT
INPUT = "G:/Geteilte Ablagen/impact_evaluation_paper/.rda/"


load(paste0(INPUT, "pollution_all_synth.rda"))
load(paste0(INPUT, "pollution_all_synth_wide.rda"))


synth_street <- function(street){

  # filter out the other street
if(street == "Max-Brauer-Allee (Straße)"){
  df_wide <- df_wide %>% filter(station_name != "Stresemannstraße")
} else{
  df_wide <- df_wide %>% filter(station_name != "Max-Brauer-Allee (Straße)")
}
# choose treatment date id & maximum date id (interactive, if time window is flexible)
date_treat <- df_wide %>% filter(year_month == "2018-06-01") %>% pull(date_id) %>% unique()
date_max <- df_wide %>% pull(date_id) %>% unique() %>% max()

# choose station id (interactive)
station_treat <- df_wide %>% filter(station_name == {{street}}) %>% pull(station_id) %>% unique()
station_cont <- df_wide %>% filter(station_name != {{street}}) %>% pull(station_id) %>% unique()


dataprep.out <- dataprep(as.data.frame(df_wide),
                         predictors = c("nitrogen dioxide", "PM10"),
                         dependent = "PM10",
                         unit.variable = "station_id",
                         time.variable = "date_id",
                         #unit.names.variable = "Y",
                         treatment.identifier = station_treat,
                         controls.identifier = station_cont,
                         time.predictors.prior = c(1:25),
                         time.optimize.ssr = c(1:date_treat),
                         time.plot = c(1:date_max))


synth.out <- synth(dataprep.out)


path.plot(synth.res    = synth.out,
          dataprep.res = dataprep.out,
          Ylab         = c("Y"),
          Xlab         = c("Year"),
          Legend       = c(street ,paste0("Synthetic ", street)),
          Legend.position = c("topleft")
)


abline(v   = date_treat,
       lty = 2)

title("Synthetic Control apporach")

}

synth_street("Stresemannstraße")
synth_street("Max-Brauer-Allee (Straße)")
