# I will reproduce the error like in the INCLINE script, that is the only way I think at this point

library(dataDownloader)
# library(fluxible)
library(tidyverse)
library(fs)
# library(lubridate)
# library(broom)
# library(zoo)

# source("https://raw.githubusercontent.com/jogaudard/common/master/fun-fluxes.R")

# download and read data -----------------------------------------------------------
get_file(node = "zhk3m",
         file = "INCLINE_CO2_2022.zip",
         path = "incline_test/raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2022.csv",
         path = "incline_test/raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "incline_test/raw_data",
         remote_path = "RawData")

# Unzip files
zipfile <- "incline_test/raw_data/INCLINE_CO2_2022.zip"
if(file.exists(zipfile)){
  outdir <- "incline_test/raw_data"
  unzip(zipfile, exdir = outdir)
}

#importing fluxes data
location <- "incline_test/raw_data" #location of datafiles

raw_CO2_INCLINE_2022 <- dir_ls(location, regexp = "*CO2_campaign*")  |>
  map_dfr(read_csv,  na = c("#N/A", "Over")) |>
  rename( #rename the column to get something more practical without space
    CO2 = "CO2 (ppm)",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    PAR = "PAR (umolsm2)",
    datetime = "Date/Time"
  ) %>%  
  mutate(
    datetime = dmy_hms(datetime)
  ) %>%
  select(datetime, CO2, PAR, temp_air, temp_soil)

record <- read_csv("incline_test/raw_data/INCLINE_field-record_2022.csv", na = c(""), col_types = "fffccfc") %>% 
  drop_na(starting_time) %>%  #delete row without starting time (meaning no measurement was done)
  mutate(
    starting_time = case_when(
      campaign != 1 ~ gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # campaing 1 was written as hh:mm:ss and others as hhmmss
      campaign == 1 ~ starting_time
      ),
    start = paste(date, starting_time),
    start = ymd_hms(start)
  )

str(raw_CO2_INCLINE_2022)
str(record)

# match fluxes and CO2 concentration --------------------------------------

CO2_INCLINE_2022 <- flux_match(
  raw_CO2_INCLINE_2022,
  record,
  startcrop = 0,
  measurement_length = 180,
  conc_col = "CO2"
)

# CO2_INCLINE_2022  |>
#   filter(
#     f_fluxid %in% c(760:772) 
#   ) |>
#     view()
# those measurements where done in the 60 minutes before I fell in the river with the setup. which is probably why the data are missing.




# comments and temperature cleaning ---------------------------------------

CO2_INCLINE_2022 %>%
  select(comments) %>% 
  distinct()

CO2_INCLINE_2022 <- CO2_INCLINE_2022 %>% 
  mutate(
    temp_soil = case_when(
      comments %in% c("soilT logger not plugged in", "no soil T") ~ NA,
      TRUE ~ temp_soil
    )
  )

# get slopes --------------------------------------------------------------

slopes_INCLINE_2022 <- CO2_INCLINE_2022 |>
  flux_fitting(fit_type = "exp")

str(slopes_INCLINE_2022)

slopes_INCLINE_2022 <- slopes_INCLINE_2022 |>
  flux_quality(fit_type = "exp", slope_col = "f_slope_tz")


slopes_INCLINE_2022 |>
filter(
    f_fluxid %in% c(1:240)
) |>
# view()
flux_plot(
  fit_type = "exp",
  f_ylim_lower = 300,
#   f_ylim_upper = 240919.411,
  f_plotname = "incline_test.pdf"
#   f_ncol = 1,
#   f_nrow = 1
  )

# the problem is when the slope is crazy (b out of range), there is only one point for fit_slope inside the limit and I get a warning
# one solution is to replace by NA all values outside of the range of the plot

# another idea: everything in black so no problem with color, but one file per quality flag