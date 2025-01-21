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

# reproducing group aesthetics error
slopes0lin_flag |>
filter(f_fluxID == 1) |>
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

# missing data issue with flux_fitting_exp
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


try_segment <- pftc7_short |>
    group_by(.data$file_name) |>
    mutate(
      f_time = difftime(.data$date_time[seq_along(.data$date_time)],
        .data$date_time[1],
        units = "secs"
      ),
      f_time = as.double(.data$f_time),
      ID = dplyr::cur_group_id()
    ) |>
    ungroup()

    str(try_segment)

for(fluxid in unique(try_segment$ID)){
  dt_sub <- try_segment |>
    filter(ID == fluxid)
}

  res <- cpop(try_segment$co2_conc, minseglen = 30)
  seg <- fitted(res)

pftc7_short <- pftc7_short |>
    mutate(
      f_end = start_time + 120
    )

    test_segment <- flux_fitting(
      pftc7_short,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 6,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    )

str(test_segment)
View(test_segment)

test_seg_quality <- flux_quality(
  test_segment,
  par_threshold = 650,
  sd_threshold = 10
)

pftc7 <- pftc7_long |>
  group_by(file_name) |>
  mutate(
    f_end = start_time + 120,
    flux_ID = cur_group_id(),
    ndata = n()
  ) |>
  ungroup()

  str(pftc7)

pftc7 |>
group_by(file_name) |>
count()

pftc7_summary <- pftc7 |>
  group_by(file_name) |>
  nest() |>
  summarise(
    ndata = nrow(co2_conc)
  ) |>
  ungroup()

View(pftc7_summary)

# pftc7 <- pftc7 |>
#   filter(flux_ID %in% c(1:4))

slopes_pftc7 <- pftc7_long |>
  # filter(flux_ID %in% c(2:5)) |>
  flux_fitting(
  # conc_df = pftc7,
  min_seg_length = 30,
  start_cut = 0,
  end_cut = 0,
  start_col = "start_time",
  end_col = "f_end",
  datetime_col = "date_time",
  conc_col = "co2_conc",
  fluxid_col = "file_name",
  sign_str_col = "signal_strength",
  par_col = "par",
  h2o_col = "h2o_conc",
  fit_type = "segments"
)

pftc7 |>
filter(flux_ID == 4) |>
View()

flags_pftc7 <- flux_quality(
  slopes_df = slopes_pftc7,
  ambient_conc = 421,
  error = 100,
  fluxid_col = "f_fluxID",
  slope_col = "f_slope",
  pvalue_col = "f_pvalue",
  rsquared_col = "f_rsquared",
  f_flag_fit_col = "f_flag_fit",
  par_threshold = 600,
  sign_str_threshold = 98,
  pvalue_threshold = 0.3,
  rsquared_threshold = 0.7,
  sd_threshold = 0,
  ratio_threshold = 0,
  conc_col = "f_conc",
  time_col = "f_time",
  fit_col = "f_fit",
  cut_col = "f_cut",
  cut_arg = "cut"
)

str(flags_pftc7)
View(flags_pftc7)

flags_pftc7 |>
filter(f_fluxID == "5_2800_east_2_day_photo.txt") |>
View()

flags_pftc7_try <- flags_pftc7 |>
  group_by(f_fluxID) |>
  count(f_quality_flag_seg) |>
  top_n(1)

flags_pftc7_try

segment_flag <- flags_pftc7 |>
  filter(
    f_cut != "cut"
  ) |>
  select(f_fluxID, f_quality_flag_seg) |>
    drop_na(f_quality_flag_seg) |>
    distinct() |>
    group_by(f_fluxID) |>
    mutate(
      count = n()
    ) |>
    ungroup() |>
    filter(count == 1) |>
    rename(
      f_quality_flag = "f_quality_flag_seg"
    ) |>
    select(!count)

segment_flag
