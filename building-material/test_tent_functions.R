
source("R/calc_tent_fluxes.R")
source("R/flux_segment.R")

# load

dt <- fread("data-raw/pftc7_site5_photo.csv")

## Calculate CO2 fluxes 

co2_fluxes <- calc_tent_fluxes(
  vol = 2.197, #default 
  area = 1.69, #default 
  signal_strength_tresh = 95.0,  # Threshold for valid signal strength
  par_thresh = 650,  # Photosynthetically Active Radiation (PAR) threshold
  flux_df = dt,  # Data frame containing flux data
  param = "co2",  # Parameter to analyze, default is CO2
  par_col = "par",  # Column name for PAR in flux_df
  date_time_col = "date_time",  # Column name for datetime in flux_df
  co2_col = "co2_conc",  # Column name for CO2 concentration in flux_df
  h2o_col = "h2o_conc",  # Column name for H2O concentration in flux_df
  signal_strength_col = "signal_strength",  # Column name for signal strength in flux_df
  temp_col = "temperature_c",  # Column name for air temperature in flux_df
  pressure_col = "pressure_kpa",  # Column name for pressure in kPa in flux_df
  flux_type_col = "measurement",  # Column name for flux type in flux_df
  plot_id_col = "plot_id",  # Column name for plot ID in flux_df
  redo_col = "redo", 
  day_night_col = "day_night",
  file_name_col = "file_name",
  start_time_col = "start_time",
  min_length = 60,  # minimum flux length 
  skip = 7, #number of rows to skip from each flux measurement, 
  correct_for_h2o_conc = TRUE, 
  min_seg_length = 30
)

str(co2_fluxes)

fwrite(co2_fluxes, "data-raw/pftc7_site5_photo_flux_estimates_og_function.csv")


dt_seg <- flux_segment( 
    signal_strength_tresh = 95.0,  # Threshold for valid signal strength
    par_thresh = 650,  # Photosynthetically Active Radiation (PAR) threshold
    flux_df = dt,  # Data frame containing flux data
    param = "co2",  # Parameter to analyze, default is CO2
    par_col = "par",  # Column name for PAR in flux_df
    date_time_col = "date_time",  # Column name for datetime in flux_df
    co2_col = "co2_conc",  # Column name for CO2 concentration in flux_df
    h2o_col = "h2o_conc",  # Column name for H2O concentration in flux_df
    signal_strength_col = "signal_strength",  # Column name for signal strength in flux_df
    flux_type_col = "measurement",  # Column name for flux type in flux_df
    day_night_col = "day_night",
    flux_id_col = "file_name",
    start_time_col = "start_time",
    min_length = 60,  # minimum flux length 
    skip = 7, #number of rows to skip from each flux measurement, 
    correct_for_h2o_conc = TRUE, 
    min_seg_length = 30)

fwrite(dt_seg, "data-raw/pftc7_site5_photo_flux_segment_output.csv")
