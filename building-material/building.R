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
# co2_df_missing$CO2[c(FALSE, TRUE)] <- NA # we replace every second row with NA in CO2 to make it incomplete (less than 50% of data not NA)

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

slopes0 <- flux_fitting_zhao18(co2_conc)


   
slopes60 <- co2_conc %>%
   flux_fitting_zhao18(
      end_cut = 60
   )

slopes30 <- co2_conc %>%
   flux_fitting_zhao18(
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

# reproducing group aesthetics error
slopes0lin_flag |>
filter(f_fluxid == 1) |>
flux_plot(fit_type = "lin",
            f_ylim_upper = 432)


view(slopes0_flag)
max(slopes0_flag$f_conc)

library(progress)
pb <- progress_bar$new(total = 100)
purrr::walk(1:100, flux_plot(slopes0lin_flag, fit_type = "lin"))
foo <- flux_plot()

foo2 <- function(slopes_df, fit_type){


pb <- progress_bar$new(total = 100)



foo <- function(x){
   
  pb$tick()

  Sys.sleep(0.1)
   #  flux_plot(slopes_df = ((slopes_df)), fit_type = ((fit_type)))


}



purrr::walk(1:100, foo)
flux_plot(slopes_df = ((slopes_df)), fit_type = ((fit_type)))


}

foo2(slopes0_flag, fit_type = "exp")


testdf <- tibble(
   a = c(1:10),
   b = c(11:20)
)
testdf
pb <- progress_bar$new(total = 10)

for(i in 1:10){
     pb$tick()
  Sys.sleep(0.1)

   testdf$c[i] = testdf$a[i] * testdf$b[i]
}

testdf

pb <- progress_bar$new(total = 100)
for (i in 1:100) {
  pb$tick()
  i + 1
}


flux_plot(slopes0_flag, fit_type = "exp", f_ncol = 1, f_nrow = 1, f_ylim_upper = 430)


# trying some attribute stuff
slopes_test <- flux_fitting(co2_conc, fit_type = "exp")
str(slopes_test)
attr_slopes_test <- attributes(slopes_test)
attr_slopes_test$fit_type
attributes(slopes30qua)$fit_type
str(co2_fluxes)

# problem with the fun check

test_fun2 <- function(arg){
   arg * 2
}

flux_fun_check_test <- function(
                           arg_numeric = c()) {
  type <- c()
for(i in seq_along(arg_numeric)) {
  type[i] <- class(eval(as.symbol(arg_numeric[i])))
}
  arg_df <- tibble(
    name = ((arg_numeric)),
    type = type,
    supposed_type = "numeric"
  )


arg_df


}

arg1 <- 12
arg2 <- "blop"

flux_fun_check_test(arg_numeric = c("arg1", "arg2"))


test_function <- function(df,
                        arg_test1 = 10,
                        arg_test2 = "gneu"
                        ){
                           # environment(flux_fun_check_test) <- environment()
                           flux_fun_check_test(
                              arg_numeric = c("arg_test1", "arg_test2")
                           )
                           # test_fun2(arg = arg1)
                        
                           # head(df)
                        }
                        
                        # vec <- names(formals(test_function))
                        # vec
                        # environment(test_function)
test_function(slopes0)

# missing data issue with flux_fitting_zhao18
test_df <- flux_fitting(co2_conc,
   fit_type = "expo",
   end_cut = 30)
View(test_df)

test_df  |>
   flux_quality(slope_col = "f_slope") |>
      flux_plot()


# gep warning message trials
campaign <- c(1, 1, 2, 2, 3, 3, 3, 4, 4)
  turfid <- c("A", "A", "A", "A", "B", "B", "C", "D", "A")
  type <- c("NEE", "ER", "NEE", "ER", "NEE", "ER", "ER", "ER", "ER")
  flux <- c(3, 5, 2, 7, 9, 11, 10, 13, 8)
  datetime <- c(
    "2024-02-11 10:00:00",
    "2024-02-11 10:00:10",
    "2024-02-11 10:00:20",
    "2024-02-11 10:00:30",
    "2024-02-11 10:00:40",
    "2024-02-11 10:00:50",
    "2024-02-11 10:01:00",
    "2024-02-11 10:01:10",
    "2024-02-11 10:01:20"
  )
  PAR <- c(300, 2, 250, 5, 320, 1, 0, 3, 4)

  fluxes <- tibble(
    campaign,
    turfid,
    type,
    flux,
    datetime,
    PAR
  )
fluxes
id_cols <- c("campaign", "turfid")

fluxes_gep <- fluxes |>
    select(
      "flux",
      "type",
      "datetime",
      "PAR",
      all_of(((id_cols)))
    ) |> filter(
      .data$type == "NEE" |
        .data$type == "ER"
    )

  fluxes_gep <- fluxes_gep |>
    pivot_wider(id_cols = all_of(((id_cols))),
      names_from = "type",
      values_from = c("flux", "datetime", "PAR")
    ) |>
    rename(
      ER = "flux_ER",
      NEE = "flux_NEE",
      PAR = "PAR_NEE",
      datetime = "datetime_NEE"
    ) |>
    mutate(
      flux = .data$NEE - .data$ER,
      type = "GEP"
    ) |>
    select(
      "datetime",
      all_of(((id_cols))),
      "PAR",
      "type",
      "flux"
    )

fluxes_gep

  # col_id <- paste(".data$", id_cols, sep = "")
  # str(col_id)

  nee_missing <- fluxes_gep |>
    filter(
      is.na(.data$datetime)
    ) |>
    select(all_of(((id_cols))))
    mutate(
      rowid = row_number()
    )

nrow(nee_missing)
nee_missing[[1]]
# nee_missing <- nee_missing |>
#   mutate(
#   msg_id <- c(seq_len(nrow(nee_missing)))
# msg_id <- tibble(
msg_id <- c()

# )
    for(i in seq_along(((id_cols)))) {
  msg_id[i] <- paste(id_cols[[i]], nee_missing[[i]], collapse = ", ")
   # msg_id[1] <- c(paste(id_cols[[i]], nee_missing[[i]]))
}
msg_id

# nee_missing_try <- nee_missing |>
#    mutate(
#       msg = for(i in seq_along(((id_cols)))) {
#   paste(id_cols[[i]], nee_missing[[i]], sep = ": ")
# }
#    )
# nee_missing_try

nee_missing_try <- nee_missing |>
   mutate(
      rowid = row_number(),

   ) |>
   pivot_longer(all_of(id_cols))

nee_missing_try

msg_id[1] <- paste(colnames(nee_missing[1]), nee_missing[1])
 nee_missing[1, "turfid"]

nee_missing <- nee_missing |>
   mutate(msg = NA)

# msg <- tibble()

for(i in seq_along(((id_cols)))) {
for(n in seq_along(nrow(nee_missing))) {
   # nee_missing[n, "msg"] <- paste(colnames(nee_missing[i]), nee_missing[n, i])
   nee_missing[n, i] <- paste(colnames(nee_missing[i]), nee_missing[[n, i]])

}
}
# msg

# for(i in seq_along(((id_cols)))) {
for(i in 1:2) {
# for(n in seq_along(nrow(nee_missing))) {
   # nee_missing[n, "msg"] <- paste(colnames(nee_missing[i]), nee_missing[n, i])
   # nee_missing[[n, "msg"]] <- paste(colnames(nee_missing[1]), nee_missing[[n, 1]])
   nee_missing <- nee_missing |>
      mutate(
         msg = paste(colnames(nee_missing[i]), collapse = ", ")
      )

}

df[] <- Map(paste, names(df), df, sep = ':')
nee_missing
nee_missing[] <- Map(paste, names(nee_missing), nee_missing, sep = ": ")
nee_missing

nee_missing_try <- nee_missing |>
    mutate(
      msg = apply(nee_missing[, id_cols], 1, paste, collapse = ", "),
      f_warning = paste(
        "\n", "NEE missing for measurement", msg
      )
    )
nee_missing_try

flux_plot(slopes0_flag)
flux_plot(slopes0lin_flag, output = "pdfpages")


testing <- function(conc_df,
                    start_col,
                    end_col
) {
  df <- conc_df |>
        dplyr::mutate(
          test = int_length(interval({{start_col}}, {{end_col}}))
        ) |>
        dplyr::select(test) |>
        max()

        df
}

testing(co2_conc, f_start, f_end)

testing_lm <- function(conc_df,
                       start_col,
                       end_col,
                       datetime_col,
                       conc_col,
                       fluxid_col,
                       start_cut = 0,
                       end_cut = 0,
                       fit_type = "exp"
                       ) {
  # conc_df <- conc_df |>
  #   mutate(
  #     time = difftime({{datetime_col}}[seq_along({{datetime_col}})],
  #       {{datetime_col}}[1],
  #       units = "secs"
  #     ),
  #     time = as.double(.data$time),
  #     .by = {{fluxid_col}}
  #   )

conc_df <- conc_df |>
    group_by({{fluxid_col}}) |>
    distinct({{datetime_col}}, .keep_all = TRUE) |>
    ungroup()

    fit_type <- flux_fit_type(
    conc_df,
    fit_type = fit_type
  )

length_flux_max <- conc_df |>
    mutate(
      length_flux = int_length(interval({{start_col}}, {{end_col}}))
      # length_flux = interval({{end_col}}, {{start_col}})
      # length_flux = as.double(.data$length_flux)
    ) |>
    select("length_flux") |>
    max()

  if ((start_cut + end_cut) >= length_flux_max) {
    stop(
      "You cannot cut more than the length of the measurements!"
    )
  }

  conc_df <- conc_df |>
    mutate(
      f_time = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time = as.double(.data$f_time),
      {{start_col}} := {{start_col}} + start_cut,
      {{end_col}} := {{end_col}} - end_cut,
      f_cut = case_when(
        {{datetime_col}} < {{start_col}} | {{datetime_col}} >= {{end_col}}
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      n_conc = sum(!is.na({{conc_col}})),
      .by = {{fluxid_col}}
    )

  conc_df_cut <- conc_df |>
    filter(
      .data$f_cut == "keep"
    ) |>
    drop_na({{conc_col}}) |>
    mutate(
      f_time_cut = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time_cut = as.double(.data$f_time_cut),
      length_window = max(.data$f_time_cut),
      length_flux = difftime({{end_col}}, {{start_col}}, units = "sec"),
      time_diff = .data$f_time - .data$f_time_cut,
      n_conc_cut = sum(!is.na({{conc_col}})),
      .by = {{fluxid_col}}
    )

cm_temp <- conc_df_cut |>
    group_by({{fluxid_col}}) |>
    distinct({{conc_col}}, .keep_all = TRUE) |>
    mutate(
      Cmax = max({{conc_col}}),
      Cmin = min({{conc_col}}),
      tmax = .data$f_time_cut[{{conc_col}} == .data$Cmax],
      tmin = .data$f_time_cut[{{conc_col}} == .data$Cmin]
    ) |>
    select({{fluxid_col}}, "Cmax", "Cmin", "tmax", "tmin") |>
    ungroup() |>
    distinct(.data$Cmax, .data$Cmin, .keep_all = TRUE)

    cm_slope <- conc_df_cut |>
      group_by({{fluxid_col}}) |>
      nest() |>
      mutate(
        mod_lm = map(.x = data, \(.x) lm(rlang::expr(!! rlang::enexpr(conc_col) ~ f_time_cut), data = .x)),
        tidy = map(mod_lm, broom::tidy)
    ) |>
    unnest(tidy) |>
    filter(term == "f_time_cut") |>
    rename(slope = "estimate") |>
    unnest(cols = c({{fluxid_col}}, data)) |>
    select({{fluxid_col}}, slope) |>
    distinct()

    cm_slope
    # conc_df_cut

                       }

testing_lm(co2_conc, f_start, f_end, f_datetime, f_conc, f_fluxid)

debug(flux_fitting_zhao18)

flux_fitting_zhao18(co2_conc,
                 f_start,
                 f_end,
                 f_datetime,
                 f_conc,
                 f_fluxid,
                         start_cut = 0,
                         end_cut = 0,
                         t_window = 20,
                         cz_window = 15,
                         b_window = 10,
                         a_window = 10,
                         roll_width = 15)  |>
                         select(f_fluxid, f_slope, Cm_est) |>
                         distinct()

flux_fitting(co2_conc,
                 f_start,
                 f_end,
                 f_datetime,
                 f_conc,
                 f_fluxid,
                 fit_type = "exponential")


testing_lm2 <- function(conc_df,
                         start_col,
                         end_col,
                         datetime_col,
                         conc_col,
                         fluxid_col,
                         start_cut = 0,
                         end_cut = 0,
                         t_window = 20,
                         cz_window = 15,
                         b_window = 10,
                         a_window = 10,
                         roll_width = 15,
                         fit_type
                         ) {

args_ok <- flux_fun_check(list(
    start_cut = start_cut,
    end_cut = end_cut
  ),
  fn = list(is.numeric, is.numeric),
  msg = rep("has to be numeric", 2))

  conc_df_check <- conc_df |>
    select(
      {{conc_col}},
      {{start_col}},
      {{end_col}},
      {{datetime_col}}
    )

  conc_df_ok <- flux_fun_check(conc_df_check,
                               fn = list(
                                 is.numeric,
                                 is.POSIXct,
                                 is.POSIXct,
                                 is.POSIXct
                               ),
                               msg = rep(c(
                                 "has to be numeric",
                                 "has to be POSIXct"
                               ),
                               c(1, 3)
                               ),
                               origdf = conc_df)


  if (any(!c(args_ok, conc_df_ok)))
    stop("Please correct the arguments", call. = FALSE)

  # conc_df <- conc_df |>
  #   rename(
  #     f_start = all_of((start_col)),
  #     f_end = all_of((end_col)),
  #     f_datetime = all_of((datetime_col)),
  #     f_conc = all_of((conc_col)),
  #     f_fluxid = all_of((fluxid_col))
  #   )

  conc_df <- conc_df |>
    group_by({{fluxid_col}}) |>
    distinct({{datetime_col}}, .keep_all = TRUE) |>
    ungroup()

  fit_type <- flux_fit_type(
    conc_df,
    fit_type = fit_type
  )

  args_ok <- flux_fun_check(list(
    t_window = t_window,
    cz_window = cz_window,
    b_window = b_window,
    a_window = a_window,
    roll_width = roll_width,
    start_cut = start_cut,
    end_cut = end_cut
  ),
  fn = list(
    is.numeric,
    is.numeric,
    is.numeric,
    is.numeric,
    is.numeric,
    is.numeric,
    is.numeric
  ),
  msg = rep("has to be numeric", 7))

  if (any(!args_ok))
    stop("Please correct the arguments", call. = FALSE)




  length_flux_max <- conc_df |>
    mutate(
      length_flux = int_length(interval({{start_col}}, {{end_col}}))
      # length_flux = interval({{end_col}}, {{start_col}})
      # length_flux = as.double(.data$length_flux)
    ) |>
    select("length_flux") |>
    max()

  if ((start_cut + end_cut) >= length_flux_max) {
    stop(
      "You cannot cut more than the length of the measurements!"
    )
  }

  message("Cutting measurements...")

  conc_df <- conc_df |>
    mutate(
      f_time = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time = as.double(.data$f_time),
      {{start_col}} := {{start_col}} + start_cut,
      {{end_col}} := {{end_col}} - end_cut,
      f_cut = case_when(
        {{datetime_col}} < {{start_col}} | {{datetime_col}} >= {{end_col}}
        ~ "cut",
        TRUE ~ "keep"
      ),
      f_cut = as_factor(.data$f_cut),
      n_conc = sum(!is.na({{conc_col}})),
      .by = {{fluxid_col}}
    )

  conc_df_cut <- conc_df |>
    filter(
      .data$f_cut == "keep"
    ) |>
    drop_na({{conc_col}}) |>
    mutate(
      f_time_cut = difftime({{datetime_col}}[seq_along({{datetime_col}})],
        {{datetime_col}}[1],
        units = "secs"
      ),
      f_time_cut = as.double(.data$f_time_cut),
      length_window = max(.data$f_time_cut),
      length_flux = difftime({{end_col}}, {{start_col}}, units = "sec"),
      time_diff = .data$f_time - .data$f_time_cut,
      n_conc_cut = sum(!is.na({{conc_col}})),
      .by = {{fluxid_col}}
    )

  message("Estimating starting parameters for optimization...")

  cm_temp <- conc_df_cut |>
    group_by({{fluxid_col}}) |>
    distinct({{conc_col}}, .keep_all = TRUE) |>
    mutate(
      Cmax = max({{conc_col}}),
      Cmin = min({{conc_col}}),
      tmax = .data$f_time_cut[{{conc_col}} == .data$Cmax],
      tmin = .data$f_time_cut[{{conc_col}} == .data$Cmin]
    ) |>
    select({{fluxid_col}}, "Cmax", "Cmin", "tmax", "tmin") |>
    ungroup() |>
    distinct(.data$Cmax, .data$Cmin, .keep_all = TRUE)

cm_slope <- conc_df_cut |>
      group_by({{fluxid_col}}) |>
      nest() |>
      mutate(
        mod_lm = map(.x = data, \(.x) lm(rlang::expr(!! rlang::enexpr(conc_col) ~ f_time_cut), data = .x)),
        tidy = map(mod_lm, broom::tidy)
    ) |>
    unnest(tidy) |>
    filter(term == "f_time_cut") |>
    rename(slope = "estimate") |>
    unnest(cols = c({{fluxid_col}}, data)) |>
    select({{fluxid_col}}, slope)

    cm_slope
}

testing_lm2(co2_conc, f_start, f_end, f_datetime, f_conc, f_fluxid, fit_type = "exp")

test_df <- tibble(
  ID = c(1:10),
  values = c(11:20),
  values2 = c(31:40)
)

test_fct <- function(df, col1, col2 = values2){
  df |>
    mutate(
      mean = mean({{col1}}),
      sum = sum({{col2}})
  )
}

test_fct(test_df, values)


test_vol <- function(
  df,
  col1,
  volume
){

  # if(is.double(volume)) {
  #   df <- df |>
  #     mutate(
  #       volume = volume
  #     )
  # }
name_vol <- deparse(substitute(volume))

  # name <- c(as.character(volume))
  # name

  df |>
  select({{col1}}, any_of(name_vol)) |>
  mutate(
    result = {{col1}} + {{volume}}
  )
}

test_vol(test_df, values, values2)
test_vol(test_df, values, 10)
names(test_df)

slopes30qua_flag |>
filter(f_fluxid == 1) |>
flux_plot(conc, datetime)

test <- co2_conc |>
  filter(f_fluxid == 1) |>
    slice(-c(10, 15, 20))


flux_fitting(test,
                 conc,
                 datetime,
                 fit_type = "exponential") |>
                 View()



# trying new exp_tz fit

flux_fitting(
      co2_conc,
      conc,
      datetime,
      fit_type = "exp_tz",
      end_cut = 60,
      t_zero = 20
    ) |>
    flux_quality(conc) |>
    flux_plot(conc, datetime)

# testing behaviour on a larger dataset

flux_match(
  co2_liahovden,
  record_liahovden,
  datetime,
  start,
  conc,
  startcrop = 0,
  measurement_length = 220,
  ratio_threshold = 0.5,
  time_diff = 0
) |>
  flux_fitting(
      conc,
      datetime,
      fit_type = "exp_tz",
      end_cut = 60,
      t_zero = 20
    ) |>
    flux_quality(conc) |>
    flux_plot(conc, datetime, output = "pdfpages")

# what about missing data

flux_fitting(
      co2_conc_missing,
      conc,
      datetime,
      fit_type = "exp_zhao18",
      end_cut = 60,
      t_zero = 20
    ) |>
    flux_quality(conc) |>
    # View()
    flux_plot(conc, datetime)
