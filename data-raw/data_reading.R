# for matching
co2_conc <- readr::read_csv(
  "data-raw/co2_conc.csv",
  col_types = "TddddffTTfddc"
) |>
  dplyr::rename(
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_n_conc = "n_conc",
    f_ratio = "ratio",
    f_flag_match = "flag"
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
    f_start = "start",
    f_fluxid = "fluxID",
    f_end = "end",
    f_n_conc = "n_conc",
    f_ratio = "ratio",
    f_flag_match = "flag"
  )
usethis::use_data(co2_conc_missing, overwrite = TRUE)

# for fluc calc
co2_fluxes <- readr::read_csv(
  "data-raw/fluxes.csv",
  col_types = "fdddddffTdd"
) |>
  dplyr::rename(
    f_start = "start",
    f_fluxid = "fluxID",
    f_slope_tz = "slope_tz",
    f_temp_air_ave = "temp_air_ave",
    f_flux = "flux"
  )
usethis::use_data(co2_fluxes, overwrite = TRUE)

# other temperature units
slopes0_temp <- flux_fitting(
  co2_conc,
  conc,
  datetime,
  fit_type = "exp_zhao18"
) |>
  flux_quality(
    conc
  ) |>
  mutate(
    temp_fahr = (temp_air * 1.8) + 32,
    temp_kelvin = temp_air + 273.15
  )
usethis::use_data(slopes0_temp, overwrite = TRUE)


# more missing data

co2_conc_mid_missing <- co2_conc |>
  dplyr::mutate(
    conc = replace(
      conc,
      c(
        10:20,
        325:380,
        655:670,
        700:730
      ),
      NA
    )
  )

usethis::use_data(co2_conc_mid_missing, overwrite = TRUE)
