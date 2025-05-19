
source("fluxible/R/calc_tent_fluxes.R")
source("fluxible/R/check_tent_fluxes.R")

# load

dt <- readRDS("fluxible/data/example_data/south_africa_raw_gas_conc_site_5.Rds")

## Calculate CO2 fluxes 

co2_fluxes <- calc_tent_fluxes(
  vol = 2.197, #default 
  area = 1.69, #default 
  sigStrengthThresh = 95.0, #default 
  parThresh = 650, #default 
  fluxDF = dt,  
  param = "co2", #default 
  parCol = "PAR", #default 
  dateTimeCol = "DateTime", #default 
  co2Col = "ConcCO2", #default 
  h2oCol = "ConcH2O", #default 
  signalStrengthCol = "SignalStrength", #default 
  tempCol = "AirTemperature", #default 
  pressureCol = "PressureKPa", #default 
  fluxTypeCol = "measurement", #default 
  plotIDCol = "plotID", #default 
  skip = 7, 
  redoCol = "redo",
  dayNightCol = "day_night"
)


## inspect CO2 fluxes 

weird_co2_flux_ids <-   co2_fluxes %>% 
  filter(fluxType == "photo" & fluxValue > 0) %>% 
  dplyr::select(filename) %>% pull()

co2_fluxes_checked <- check_tent_fluxes(
  vol = 2.197, #default 
  area = 1.69, #default 
  sigStrengthThresh = 95.0, #default 
  parThresh = 650, #default 
  fluxDF = dt,  
  fittedFluxes = co2_fluxes, 
  fluxIDs = weird_co2_flux_ids,
  param = "co2", #default 
  parCol = "PAR", #default 
  dateTimeCol = "DateTime", #default 
  co2Col = "ConcCO2", #default 
  h2oCol = "ConcH2O", #default 
  signalStrengthCol = "SignalStrength", #default 
  tempCol = "AirTemperature", #default 
  pressureCol = "PressureKPa", #default 
  fluxTypeCol = "measurement", #default 
  plotIDCol = "plotID", #default 
  skip = 7, 
  redoCol = "redo",
  dayNightCol = "day_night"
)

table(co2_fluxes_checked$methodFlag)


### calculate H2O fluxes 
h2o_fluxes <- calc_tent_fluxes(
  vol = 2.197, #default 
  area = 1.69, #default 
  sigStrengthThresh = 95.0, #default 
  parThresh = 650, #default 
  fluxDF = dt,  
  param = "h2o", #default 
  parCol = "PAR", #default 
  dateTimeCol = "DateTime", #default 
  co2Col = "ConcCO2", #default 
  h2oCol = "ConcH2O", #default 
  signalStrengthCol = "SignalStrength", #default 
  tempCol = "AirTemperature", #default 
  pressureCol = "PressureKPa", #default 
  fluxTypeCol = "measurement", #default 
  plotIDCol = "plotID", #default 
  skip = 7, 
  redoCol = "redo",
  dayNightCol = "day_night"
)

weird_h2o_flux_ids <-   h2o_fluxes %>% 
  filter(fluxType == "photo" & fluxValue < 0) %>% 
  dplyr::select(filename) %>% pull()

h2o_fluxes_checked <- check_tent_fluxes(
  vol = 2.197, #default 
  area = 1.69, #default 
  sigStrengthThresh = 95.0, #default 
  parThresh = 650, #default 
  fluxDF = dt,  
  fittedFluxes = h2o_fluxes, 
  fluxIDs = weird_h2o_flux_ids,
  param = "co2", #default 
  parCol = "PAR", #default 
  dateTimeCol = "DateTime", #default 
  co2Col = "ConcCO2", #default 
  h2oCol = "ConcH2O", #default 
  signalStrengthCol = "SignalStrength", #default 
  tempCol = "AirTemperature", #default 
  pressureCol = "PressureKPa", #default 
  fluxTypeCol = "measurement", #default 
  plotIDCol = "plotID", #default 
  skip = 7, 
  redoCol = "redo",
  dayNightCol = "day_night"
)
