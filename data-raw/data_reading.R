# for matching
co2_conc <- readr::read_csv(
  "data-raw/co2_conc.csv",
  col_types = "TddddffTTfddc"
) |>
  dplyr::rename(
#     f_datetime = "datetime",
    f_start = "start",
#     f_conc = "conc",
    f_fluxid = "fluxID",
    f_end = "end",
    f_n_conc = "n_conc"
  ) |>
  dplyr::arrange(datetime)
usethis::use_data(co2_conc, overwrite = TRUE)

co2_df_missing <- readr::read_csv(
  "data-raw/co2_df_missing.csv",
  col_types = "Tdddd",
  na = c("#N/A", "Over", "Invalid")
)
usethis::use_data(co2_df_missing, overwrite = TRUE)

co2_conc_missing <- readr::read_csv(
  "data-raw/co2_conc_missing.csv",
  col_types = "TddddffTTfddc"
) |>
  dplyr::rename(
  #   f_datetime = "datetime",
    f_start = "start",
  #   f_conc = "conc",
    f_fluxid = "fluxID",
    f_end = "end",
    f_n_conc = "n_conc"
  )
usethis::use_data(co2_conc_missing, overwrite = TRUE)

# for fitting log
slopes0 <- readr::read_csv("data-raw/slopes0.csv") |>
  dplyr::rename(
    # f_datetime = "datetime",
    # f_conc = "conc",
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_time = "time",
    f_cut = "cut",
    f_Cz = "Cz",
    f_Cm = "Cm",
    f_a = "a",
    f_b = "b",
    f_tz = "tz",
    f_slope = "slope_tz",
    f_fit = "fit",
    f_fit_slope = "fit_slope",
    f_start_z = "start_z"
  )
attr(slopes0, "fit_type") <- "exponential"
usethis::use_data(slopes0, overwrite = TRUE)

slopes30 <- readr::read_csv("data-raw/slopes30.csv") |>
  dplyr::rename(
    # f_datetime = "datetime",
    # f_conc = "conc",
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_time = "time",
    f_cut = "cut",
    f_Cz = "Cz",
    f_Cm = "Cm",
    f_a = "a",
    f_b = "b",
    f_tz = "tz",
    f_slope = "slope_tz",
    f_fit = "fit",
    f_fit_slope = "fit_slope",
    f_start_z = "start_z"
  )
attr(slopes30, "fit_type") <- "exponential"

usethis::use_data(slopes30, overwrite = TRUE)

slopes60 <- readr::read_csv("data-raw/slopes60.csv") |>
  dplyr::rename(
    # f_datetime = "datetime",
    # f_conc = "conc",
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_time = "time",
    f_cut = "cut",
    f_Cz = "Cz",
    f_Cm = "Cm",
    f_a = "a",
    f_b = "b",
    f_tz = "tz",
    f_slope_tz = "slope_tz",
    f_fit = "fit",
    f_fit_slope = "fit_slope",
    f_start_z = "start_z"
  )
usethis::use_data(slopes60, overwrite = TRUE)

# for fitting lin
slopes0lin <- readr::read_csv(
  "data-raw/slopes0lin.csv",
  col_types = "TddddffTTfddcdfddddddddddddT"
) |>
  dplyr::select(!std.error) |>
  dplyr::rename(
    # f_datetime = "datetime",
    # f_conc = "conc",
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_time = "time",
    f_cut = "cut",
    f_fit = "fit",
    f_pvalue = "p.value",
    f_rsquared = "r.squared",
    f_adj_rsquared = "adj.r.squared",
    f_slope = "slope",
    f_intercept = "intercept"
  )
usethis::use_data(slopes0lin, overwrite = TRUE)

slopes30lin <- readr::read_csv(
  "data-raw/slopes30lin.csv",
  col_types = "TddddffTTfddcdfddddddddddddT"
) |>
  dplyr::select(!std.error) |>
  dplyr::rename(
    # f_datetime = "datetime",
    # f_conc = "conc",
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_time = "time",
    f_cut = "cut",
    f_fit = "fit",
    f_pvalue = "p.value",
    f_rsquared = "r.squared",
    f_adj_rsquared = "adj.r.squared",
    f_slope = "slope",
    f_intercept = "intercept"
  )
attr(slopes30lin, "fit_type") <- "linear"
usethis::use_data(slopes30lin, overwrite = TRUE)

slopes60lin <- readr::read_csv(
  "data-raw/slopes60lin.csv",
  col_types = "TddddffTTfddcdfddddddddddddT"
) |>
  dplyr::select(!std.error) |>
  dplyr::rename(
    # f_datetime = "datetime",
    # f_conc = "conc",
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_time = "time",
    f_cut = "cut",
    f_fit = "fit",
    f_pvalue = "p.value",
    f_rsquared = "r.squared",
    f_adj_rsquared = "adj.r.squared",
    f_slope = "slope",
    f_intercept = "intercept"
  )
usethis::use_data(slopes60lin, overwrite = TRUE)

# for fluc calc
co2_fluxes <- readr::read_csv(
  "data-raw/fluxes.csv",
  col_types = "fdddddffTdd"
) |>
  dplyr::rename(
    f_start = "start",
    f_fluxid = "fluxID",
    f_slope_tz = "slope_tz"
  )
usethis::use_data(co2_fluxes, overwrite = TRUE)

# other temperature units
slopes0_temp <- slopes0 %>%
  mutate(
    temp_fahr = (temp_air * 1.8) + 32,
    temp_kelvin = temp_air + 273.15
  )
usethis::use_data(slopes0_temp, overwrite = TRUE)

# quality and graphs

slopes0lin_flag <- flux_quality(
  slopes0lin,
  conc,
  fit_type = "lin"
)
usethis::use_data(slopes0lin_flag, overwrite = TRUE)

slopes30lin_flag <- flux_quality(
  slopes30lin,
  conc
)
usethis::use_data(slopes30lin_flag, overwrite = TRUE)

slopes0_flag <- flux_quality(
  slopes0,
  conc
)
usethis::use_data(slopes0_flag, overwrite = TRUE)

slopes30_flag <- flux_quality(
  slopes30,
  conc
)
usethis::use_data(slopes30_flag, overwrite = TRUE)
