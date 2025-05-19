### create data subsets ### 

library(data.table)
library(tidyverse)

dt <- readRDS("data/south_africa_raw_gas_conc_site_5.Rds") %>% 
  rename(co2_conc = ConcCO2, 
         h2o_conc = ConcH2O, 
         temperature_c = AirTemperature, 
         pressure_kpa = PressureKPa, 
         signal_strength = SignalStrength, 
         date_time = DateTime, 
         start_time = StartTime, 
         file_path = Filename, 
         file_name = file, 
         plot_id = plotID, 
         par = PAR) %>% 
  dplyr::select(-"POSIXct", -"file_path")

str(dt)
unique(dt$file_name)


dt_small <- dt %>% 
  filter(measurement %in% c("photo")) 

fwrite(dt_small, "data-raw/pftc7_site5_photo.csv")

fwrite(dt, "data-raw/pftc7_site5.csv")


