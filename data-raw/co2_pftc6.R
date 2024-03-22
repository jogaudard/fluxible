library(tidyverse)
library(dataDownloader)

# getting data for example
# fetching the data
get_file(
  node = "fcbw4",
  file = "PFTC6_CO2_liahovden_2022.csv",
  path = "data-raw",
  remote_path = "raw_data/v. c_flux_raw_data"
)


# making the dataset we want
co2_liahovden <- read_csv("data-raw/PFTC6_CO2_liahovden_2022.csv", col_types = "ccdddddd", na = c("#N/A", "Over", "Invalid"))

# some renaming because logger is giving them annoying names
co2_liahovden <- co2_liahovden %>%
  rename(
    datetime = "Date/Time",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    conc = "CO2 (ppm)",
    PAR = "PAR (umolsm2)"
  ) %>%
  mutate(
    datetime = dmy_hms(datetime)
  ) %>%
  select(datetime, temp_air, temp_soil, conc, PAR) # we keep just the variables we need

usethis::use_data(co2_liahovden, overwrite = TRUE)

# we also need the field record with the meta data
get_file(
  node = "fcbw4",
  file = "PFTC6_cflux_field-record_liahovden.csv",
  path = "data-raw",
  remote_path = "raw_data/v. c_flux_raw_data"
)

# making the dataset we want
record_liahovden <- read_csv("data-raw/PFTC6_cflux_field-record_liahovden.csv")

record_liahovden <- record_liahovden %>%
  select(turfID, type, starting_time, date) %>%
  mutate(
    starting_time = formatC(starting_time, width = 6, format = "d", flag = "0"), # to make sure all the time is 6 digits
    starting_time = gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # to add the : in the time
    date = ymd(date),
    start = ymd_hms(paste(date, starting_time)), # pasting date and time together to make datetime
  ) %>%
  select(!c(starting_time, date))

  usethis::use_data(record_liahovden, overwrite = TRUE)
