# several functions are too slow
# this script measure its running time

library(tictoc)

conc_liahovden <- flux_match(
  raw_conc = co2_liahovden, # dataframe with raw gas concentration
  field_record = record_liahovden, # dataframe with meta data
  f_datetime = datetime, # column containing date and time
  start_col = start, # start date and time of each measurement
  measurement_length = 220, # length of measurements (in seconds)
  fixed_length = TRUE, # the length of the measurement is a constant
  time_diff = 0 # time difference between f_datetime and start_col
)

tic("flux_plot")

