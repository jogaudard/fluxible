# for matching
co2_conc <- readr::read_csv("data-raw/co2_conc.csv", col_types = "TddddffTTfddc") %>%
# co2_conc <- readr::read_csv("data-raw/co2_conc.csv") %>%

   dplyr::arrange(datetime)
usethis::use_data(co2_conc, overwrite = TRUE)

co2_df_missing <- readr::read_csv("data-raw/co2_df_missing.csv", col_types = "Tdddd", na = c("#N/A", "Over", "Invalid"))
usethis::use_data(co2_df_missing, overwrite = TRUE)

co2_conc_missing <- readr::read_csv("data-raw/co2_conc_missing.csv", col_types = "TddddffTTfddc")
usethis::use_data(co2_conc_missing, overwrite = TRUE)

# for fitting log
slopes0 <- readr::read_csv("data-raw/slopes0.csv")
   # dplyr::select(!c(temp_fahr, temp_kelvin))
usethis::use_data(slopes0, overwrite = TRUE)

slopes30 <- readr::read_csv("data-raw/slopes30.csv")
   # dplyr::select(!time_diff)
usethis::use_data(slopes30, overwrite = TRUE)

slopes60 <- readr::read_csv("data-raw/slopes60.csv")
   # dplyr::select(!time_diff)
usethis::use_data(slopes60, overwrite = TRUE)

# for fitting lin
slopes0lin <- readr::read_csv("data-raw/slopes0lin.csv", col_types = "TddddffTTfddcdfddddddddddddT") |>
   dplyr::select(!std.error)
usethis::use_data(slopes0lin, overwrite = TRUE)

slopes30lin <- readr::read_csv("data-raw/slopes30lin.csv", col_types = "TddddffTTfddcdfddddddddddddT") |>
   dplyr::select(!std.error)
usethis::use_data(slopes30lin, overwrite = TRUE)

slopes60lin <- readr::read_csv("data-raw/slopes60lin.csv", col_types = "TddddffTTfddcdfddddddddddddT") |>
   dplyr::select(!std.error)
usethis::use_data(slopes60lin, overwrite = TRUE)

# for fluc calc
co2_fluxes <- readr::read_csv("data-raw/fluxes.csv", col_types = "fdddddffTdd")
usethis::use_data(co2_fluxes, overwrite = TRUE)

# other temperature units
slopes0_temp <- slopes0 %>%
   mutate(
      temp_fahr = (temp_air * 1.8) + 32,
      temp_kelvin = temp_air + 273.15
   )
usethis::use_data(slopes0_temp, overwrite = TRUE)
