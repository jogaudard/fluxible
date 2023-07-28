# this script will be to test the functions, and will also serve a an example of the workflow with this package


# packages and sources ----------------------------------------------------
library(tidyverse)
library(dataDownloader)
library(fs)

source("code/match.R")
source("code/fitting.R")
source("code/evaluation.R")
source("code/calculate.R")


# import data -------------------------------------------------------------

get_file(node = "zhk3m",
         file = "INCLINE_CO2_2022.zip",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_field-record_2022.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "zhk3m",
         file = "INCLINE_metadata.csv",
         path = "raw_data",
         remote_path = "RawData")

# Unzip files
zipfile <- "raw_data/INCLINE_CO2_2022.zip"
if(file.exists(zipfile)){
  outdir <- "raw_data"
  unzip(zipfile, exdir = outdir)
}

#importing fluxes data
location <- "raw_data" #location of datafiles

raw_CO2_INCLINE_2022 <-
  dir_ls(location, regexp = "*CO2_campaign*") %>% 
  map_dfr(read_csv,  na = c("#N/A", "Over")) %>% # this next part was initially in the match funtion, but it is more use friendly to remove it (?)
  rename( #rename the columns with easier names to handle in the code
    datetime = "Date/Time",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    CO2 = "CO2 (ppm)",
    PAR = "PAR (umolsm2)"
  ) %>% 
  mutate(
    datetime = dmy_hms(datetime), #transform the date into R date format
    temp_air = as.numeric(temp_air),
    temp_soil = as.numeric(temp_soil),
    CO2 = as.numeric(CO2),
    PAR = as.numeric(PAR),
  ) %>% 
  select(datetime, temp_soil, temp_air, CO2, PAR)

record <- read_csv("raw_data/INCLINE_field-record_2022.csv", na = c(""), col_types = "fffccfc") %>% 
  drop_na(starting_time) %>%  #delete row without starting time (meaning no measurement was done)
  mutate(
    starting_time = case_when(
      campaign != 1 ~ gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # campaing 1 was written as hh:mm:ss and others as hhmmss
      campaign == 1 ~ starting_time
    )
  )

# match data --------------------------------------------------------------


# fitting the fluxes ------------------------------------------------------


# graphs and visual assessment --------------------------------------------


# quality assessment ------------------------------------------------------


# calculation of fluxes ---------------------------------------------------


