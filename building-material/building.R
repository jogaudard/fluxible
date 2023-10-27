# script with all the steps to build the package, taken from https://biostats-r.github.io/biostats/package/
# and some random notes as well so I don't forget things

library(usethis)
library(available)
library(roxygen2)


# finding out if the package name is available ----------------------------

# wtflux sounds too offensive to native English speakers
# other ideas: fluxury, fluxhurry, fluxable, fluxible

available("fluxhurry")


# creating the structure --------------------------------------------------

# path <- "/home/jga051/Documents/01_PhD/06_projects/wtflux"
#
# create_package(path = path)



# tests to check the package ----------------------------------------------

# what do I need to test?
# should I create a fake dataset for which I know the values to test those functions? yes (Richard)
# advice from Richard: run the function on a dataset in the beginning when I am sure of what it is doing, then use this as a test. It will test if the function behaviour ahs changed.
# what about having functions dealing ONLY with fluxes, and not dragging along PAR, temperatures and co values?

usethis::use_testthat()

# creating a sample dataset to use in the test
# we can use PFTC6 data because that way we have data over midnight (special case)

library(dataDownloader)
library(tidyverse)
library(lubridate)
library(timetk)

get_file(node = "fcbw4",
         file = "PFTC6_CO2_joasete_2022.csv",
         path = "data",
         remote_path = "raw_data/c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_joasete.csv",
         path = "data",
         remote_path = "raw_data/c_flux_raw_data")

co2_df <- read_csv("data/PFTC6_CO2_joasete_2022.csv")
record <- read_csv("data/PFTC6_cflux_field-record_joasete.csv")

# we want a shorter standard dataset covering midnight
co2_df <- co2_df %>%
   rename(
    datetime = "Date/Time",
    temp_air = "Temp_air ('C)",
    temp_soil = "Temp_soil ('C)",
    CO2 = "CO2 (ppm)",
    PAR = "PAR (umolsm2)"
   ) %>%
   mutate(
      datetime = dmy_hms(datetime)
   ) %>%
         select(datetime, temp_air, temp_soil, CO2, PAR) # we keep just the variables we need

co2_df_short <- co2_df %>%
   filter( # we will just make it shorter and keep a couple of fluxes around midnight
         between_time(datetime, "2022-07-28 23:40:00", "2022-07-29 00:10:00")
         )


# same with the record file
record_short <- record %>%
   select(turfID, type, starting_time, date) %>%
      mutate(
         starting_time = gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # to add the : in the time
         date = ymd(date),
         start = ymd_hms(paste(date, starting_time)), #pasting date and time together to make datetime
      ) %>%
         select(!c(starting_time, date)) %>%
            filter( # we will just make it shorter and keep a couple of fluxes around midnight
         between_time(start, "2022-07-28 23:40:00", "2022-07-29 00:10:00")
         )



# a dataset with too many missing data
co2_df_missing <- co2_df_short
co2_df_missing$CO2[c(FALSE, TRUE)] <- NA_real_ # we replace every second row with NA in CO2 to make it incomplete (less than 50% of data not NA)

# the matching dataset that we want to have after the matching function
# we can use the matching function to build it and then manually carefully check it

# this part still needs some work: rerun match with the correct length of measurement and check the df
co2_conc <- match_flux(raw_flux = co2_df_short, field_record = record_short)

# let's store them as csv for the tests
write_csv(record_short, "data/record_short.csv")
write_csv(co2_df_short, "data/co2_df_short.csv")
write_csv(co2_df_missing, "data/co2_df_missing.csv")
write_csv(co2_conc, "data/co2_conc.csv")


# package workflow --------------------------------------------------------
# it seems that to make testing easier I need to split my functions in smaller bits (I can add some wrap-up functions later)
#
# what.ID: match fluxes with measurement ID
# will stay one function.
# this function matches continuously logged data into the different measurements and adds a measurement ID.
# it does so by using the field record provided by the user (record of which measurements was taken when with same time as the logger)
# side-note: we could include a time difference parameter in case the logger and the user were not synchronized

# fitting.fluxes: this one will be split into several functions
# what.slope: a function to calculate the slope of the flux
# what.quality : to calculate the quality parameters of each flux (RMSE, NRMSE, correlation coefficient and co)
# what.flag: to add the flags based on the quality parameters and the user's choices



# should I include a function to graph the fluxes or can we assume that people know how to use ggplot?

# what.flux: this function is only calculating the fluxes based on the slope and other needed inputs
# GPP and corrections will be other functions

# what.GPP: to calculate GPP as the difference between NEE and ER

# what.GPP_corr: to correct GPP for nighttime measurement (stable layer issue, specific to PFTC6 measurements)



































