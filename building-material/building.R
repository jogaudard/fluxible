# script with all the steps to build the package, taken from https://biostats-r.github.io/biostats/package/
# and some random notes as well so I don't forget things

library(usethis)
library(available)
library(roxygen2)


# finding out if the package name is available ----------------------------

# wtflux sounds too offensive to native English speakers
# other ideas: fluxury, fluxhurry, fluxable, fluxible

available("fluxible")


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

# to create the documentation
devtools::document()

# creating a sample dataset to use in the test
# we can use PFTC6 data because that way we have data over midnight (special case)

# commented the package loader because I want to test if packages are specified inside the functions
# library(dataDownloader)
library(tidyverse)
# library(lubridate)
# library(timetk)
# library(broom)

# to have pipes in the functions
usethis::use_pipe()

get_file(node = "fcbw4",
         file = "PFTC6_CO2_joasete_2022.csv",
         path = "data",
         remote_path = "raw_data/c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_joasete.csv",
         path = "data",
         remote_path = "raw_data/c_flux_raw_data")

co2_df <- read_csv("tests/testthat/data/PFTC6_CO2_joasete_2022.csv", col_types = "ccdddddd", na = c("#N/A", "Over", "Invalid"))
record <- read_csv("tests/testthat/data/PFTC6_cflux_field-record_joasete.csv", col_types = "ffdDccc")

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
         timetk::between_time(start, "2022-07-28 23:40:00", "2022-07-29 00:10:00")
         )



# a dataset with too many missing data
co2_df_missing <- read_csv("tests/testthat/data/co2_df_missing.csv", col_types = "Tdddd")
# co2_df_missing <- co2_df_short
# co2_df_missing$CO2[c(FALSE, TRUE)] <- NA_real_ # we replace every second row with NA in CO2 to make it incomplete (less than 50% of data not NA)

# the matching dataset that we want to have after the matching function
# we can use the matching function to build it and then manually carefully check it

co2_df_short <- readr::read_csv("tests/testthat/data/co2_df_short.csv", col_types = "Tdddd", na = c("#N/A", "Over", "Invalid"))
  record_short <- readr::read_csv("tests/testthat/data/record_short.csv", col_types = "ffT", na = "#N/A")


co2_conc2 <- flux_match(
   raw_conc = co2_df_short,
   field_record = record_short
   )

co2_conc2$turfID %>%
   unique()

   view(co2_conc2)

co2_conc_missing <- match_flux(
   co2_df_missing,
   record_short
   )

   view(co2_conc_missing)


# let's store them as csv for the tests
write_csv(record_short, "tests/testthat/data/record_short.csv")
write_csv(co2_df_short, "tests/testthat/data/co2_df_short.csv")
# write_csv(co2_df_missing, "data/co2_df_missing.csv") # will do the missing df manually to remove some data
write_csv(co2_conc, "tests/testthat/data/co2_conc.csv")
write_csv(co2_conc_missing, "tests/testthat/data/co2_conc_missing.csv")

# to test the fitting, we will use the function, graph the fluxes, check them carefully and then assume the output is the expected one
co2_conc <- readr::read_csv("tests/testthat/data/co2_conc.csv") # just to save time

slopes0 <- flux_fitting_exp(co2_conc)


   
slopes60 <- co2_conc %>%
   flux_fitting_exp(
      end_cut = 60
   )

slopes30 <- co2_conc %>%
   flux_fitting_exp(
      end_cut = 30
   )
   
# then we graph and check that it is all good
slopes0v1 <- readr::read_csv("tests/testthat/data/slopes0v1.csv")
slopes0 %>% # this one looks bad, because there is some stuff left at the end of the fluxes
  ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

  slopes30 <- slopes30  |>
  mutate(
         fit_slope = .data$slope_tz * (.data$time) + .data$Cz - .data$slope_tz * (.data$tz + .data$time_diff),

  )

slopes30  %>%
  ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

  slopes60  %>%
  ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

# we pass those as comments to avoid overwriting the files used in the tests
# write_csv(slopes0, "data-raw/slopes0.csv")
# write_csv(slopes30, "data-raw/slopes30.csv")
# write_csv(slopes60, "data-raw/slopes60.csv")


# with missing data

co2_conc_missing <- readr::read_csv("tests/testthat/data/co2_conc_missing.csv")
view(co2_conc_missing)

co2_conc_missing %>%
select(fluxID, conc, start, end) %>%
   group_by(fluxID) %>%
      reframe(
         count = sum(!is.na(conc)),
         length = difftime(end, start, unit = "secs")
      ) %>%
         distinct()

co2_conc_missing %>%
   ggplot(aes(datetime, conc)) +
   geom_point() +
   facet_wrap(~fluxID, scales = "free")

slopes_missing <- co2_conc_missing %>%
# drop_na(conc) %>%
   # filter(fluxID == 4) %>%
   flux_fitting_log(
      # end_cut = 29
      )

view(slopes_missing)

slopes_missing_10 <- flux_fitting_log(
    co2_conc_missing,
    start_cut = 10
    )


  slopes_missing  %>%
  ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   ylim(-60000,600) +
  facet_wrap(~fluxID, scales = "free")

# need to test some stuff arounf time_diff because I don't think it works the way it should

slopes10s <- flux_fitting_log(
   co2_conc,
   start_cut = 10)

slopes10s %>%
   ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   ylim(-60000,600) +
  facet_wrap(~fluxID, scales = "free")

  slopes60s <- flux_fitting_log(
   co2_conc,
   start_cut = 60)

slopes60s %>%
   ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   ylim(-60000,600) +
  facet_wrap(~fluxID, scales = "free")

flux_fitting_log(
    co2_conc,
    start_cut = 120,
    end_cut = 100
    )


fluxes <- readr::read_csv("tests/testthat/data/fluxes.csv")

co2_conc <- readr::read_csv("tests/testthat/data/co2_conc.csv") # just to save time

slopes0lin <- flux_fitting_lin(co2_conc)

slopes0lin %>%
   ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   ylim(-60000,600) +
  facet_wrap(~fluxID, scales = "free")

  slopes10lin <- flux_fitting_lin(co2_conc, end_cut = 10)

slopes10lin %>%
   ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   ylim(-60000,600) +
  facet_wrap(~fluxID, scales = "free")

  slopes30lin <- flux_fitting_lin(co2_conc, end_cut = 30)

slopes30lin %>%
   ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   ylim(-60000,600) +
  facet_wrap(~fluxID, scales = "free")

slopes60lin <- flux_fitting_lin(co2_conc, end_cut = 60)

slopes60lin %>%
   ggplot(aes(datetime)) +
  geom_point(aes(y = conc, color = cut), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red"
   #  "ok" = "black",
   #  "discard" = "red",
   #  "zero" = "grey",
   #  "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   ylim(-60000,600) +
  facet_wrap(~fluxID, scales = "free")

#   write_csv(slopes0lin, "tests/testthat/data/slopes0lin.csv")
#    write_csv(slopes10lin, "tests/testthat/data/slopes10lin.csv")
#     write_csv(slopes30lin, "tests/testthat/data/slopes30lin.csv")
#      write_csv(slopes60lin, "tests/testthat/data/slopes60lin.csv")

# to test the package
devtools::test()

# to create a new test file
usethis::use_test(name = "flux_fitting_lin")

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




# making quadratic datasets

slopes30qua <- flux_fitting(co2_conc, fit_type = "quadratic", t_zero = 10, end_cut = 30) |>
   flux_quality(fit_type = "quadratic")

# head(slopes0qua)
# str(slopes0qua)


flux_plot(slopes0qua, fit_type = "quadratic", print_plot = TRUE, f_ncol = 3)



























