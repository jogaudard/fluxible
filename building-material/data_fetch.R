library(dataDownloader)

# fetching the data
get_file(
  node = "fcbw4",
  file = "PFTC6_CO2_joasete_2022.csv",
  path = "data-raw",
  remote_path = "raw_data/v. c_flux_raw_data"
)

get_file(
  node = "fcbw4",
  file = "PFTC6_cflux_field-record_joasete.csv",
  path = "data-raw",
  remote_path = "raw_data/v. c_flux_raw_data"
)

get_file(
  node = "fcbw4",
  file = "PFTC6_cflux_field-record_liahovden.csv",
  path = "data-raw",
  remote_path = "raw_data/v. c_flux_raw_data"
)
get_file(
  node = "fcbw4",
  file = "PFTC6_CO2_liahovden_2022.csv",
  path = "data-raw",
  remote_path = "raw_data/v. c_flux_raw_data"
)