library(dataDownloader)
library(tidyverse)
library(lubridate)
library(usethis)

# IMPORTANT NOTICE
# The original files from TERRA cannot be published yet,
# so they will not be uploaded to github, meaning that
# this script can run only on Joseph's computer.

get_file(
  node = "rba87",
  remote_path = "raw_data/ecosystem_fluxes/Week_25",
  file = "CO2_CH4_2024-06-18.data",
  path = "data-raw/TERRA" # for safety, this folder is added to gitignore
)

get_file(
  node = "rba87",
  remote_path = "raw_data/ecosystem_fluxes/Week_25",
  file = "Fieldnotes.csv",
  path = "data-raw/TERRA"
)

get_file(
  node = "rba87",
  remote_path = "raw_data/ecosystem_fluxes/Week_25",
  file = "PAR_Temp_2024-06-18.dat",
  path = "data-raw/TERRA"
)

terra_conc <- read_tsv(
  "data-raw/TERRA/CO2_CH4_2024-06-18.data",
  skip = 5
) |>
  slice(-1) |> # removing row with units
  mutate( # annoying row with units passed everything as character
    co2_conc = as.numeric(CO2),
    ch4_conc = as.numeric(CH4),
    date = ymd(DATE),
    time = hms(TIME),
    datetime = as_datetime(paste(date, time))
  ) |>
  select(co2_conc, ch4_conc, datetime)

str(terra_conc)

terra_record <- read_csv(
  "data-raw/TERRA/Fieldnotes.csv"
) |>
  filter( # we take only the control so no sensitive data are released
    RAIN == "control" &
      GRUBBING == "control" &
      WARMING == "control"
  ) |>
  mutate(
    start = as_datetime(
      paste(
        DATE,
        START_TIME
      )
    )
  ) |>
  select(start)

str(terra_record)

terra_temp <- read_csv(
  "data-raw/TERRA/PAR_Temp_2024-06-18.dat",
  skip = 1
) |>
  rename(
    temp_air = "T_in_chamber",
    datetime = "TMSTAMP"
  ) |>
  select(datetime, temp_air)

str(terra_temp)

terra_conc <- left_join(
  terra_conc,
  terra_temp,
  by = "datetime"
)

str(terra_conc)

use_data(terra_record, overwrite = TRUE)
use_data(terra_conc, overwrite = TRUE)