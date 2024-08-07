library(tidyverse)

# making the dataset we want
co2_df <- read_csv("data-raw/PFTC6_CO2_joasete_2022.csv", col_types = "ccdddddd", na = c("#N/A", "Over", "Invalid"))

# we want a shorter standard dataset covering midnight
co2_df <- co2_df %>%
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

co2_df_short <- co2_df %>%
  filter( # we will just make it shorter and keep a couple of fluxes around midnight
    timetk::between_time(datetime, "2022-07-28 23:40:00", "2022-07-29 00:10:00")
  )

usethis::use_data(co2_df_short, overwrite = TRUE)
