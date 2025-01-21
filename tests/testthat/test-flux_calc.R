test_that("flux calculation is correct", {
  output <- flux_calc(slopes0,
    slope_col = "f_slope",
    conc_unit = "ppm",
    flux_unit = "mmol"
  )

  expect_equal(
    output$flux,
    co2_fluxes$flux,
    tolerance = 0.001
  )
})


test_that("averaging works", {
  output <- flux_calc(
    slopes0,
    slope_col = "f_slope",
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_avg = c("PAR", "temp_soil")
  )


  expect_snapshot(output)
})

test_that("keeping works", {
  expect_snapshot(flux_calc(
    slopes0,
    slope_col = "f_slope",
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_keep = c("turfID", "type", "f_start")
  ))
})

test_that("keeping and averaging work together", {
  expect_snapshot(flux_calc(
    slopes0,
    slope_col = "f_slope",
    conc_unit = "ppm",
    flux_unit = "mmol",
    cols_keep = c("turfID", "type", "f_start"),
    cols_avg = c("PAR", "temp_soil")
  ))
})

test_that("fahrenheit conversion works", {
  expect_snapshot(flux_calc(
    slopes0_temp,
    slope_col = "f_slope",
    conc_unit = "ppm",
    flux_unit = "mmol",
    temp_air_col = "temp_fahr",
    temp_air_unit = "fahrenheit"
  ))
})

test_that("kelvin conversion works", {
  expect_snapshot(flux_calc(
    slopes0_temp,
    slope_col = "f_slope",
    conc_unit = "ppm",
    flux_unit = "mmol",
    temp_air_col = "temp_kelvin",
    temp_air_unit = "kelvin"
  ))
})





test_that("error on air temp units", {
  expect_error(
    flux_calc(
      slopes0,
      slope_col = "f_slope",
      conc_unit = "ppm",
      flux_unit = "mmol",
      temp_air_unit = "melvin"
    ),
    "'arg' should be one of \"celsius\", \"fahrenheit\", \"kelvin\""
  )
  # Played for Laughs in the 1985 humor book "Science Made Stupid"
  # by Tom Weller, in which a table in its appendix lists such units as the
  # "arg" (the unit of work done incorrectly), the "galumph" (unit of waste
  # motion), the "lumpen" (unit of resistance to getting out of bed in the
  # morning), and the "melvin" (unit of temperature, "as measured from
  # absolutely perfect to absolutely awful"). A separate table of conversions
  # for weights and measures on the same page listed equivalencies such as
  # "325 cubebs = 1 furbish; 6 furbishes = 1 nautical smile;
  # 20 nautical smiles = 1 minor league; 3 minor leagues = 1 major league"
  # and "24 carrots = 1 pickelweight; 30 pickelweights = 1 tuna;
  # 1000 tuna = 1 short ton; 1.37 short tons = 1 tall ton".
  # (https://allthetropes.org/wiki/Fantastic_Measurement_System)
})

test_that("error that slope column is missing", {
  expect_error(
    flux_calc(
      slopes0,
      conc_unit = "ppm",
      flux_unit = "mmol"
    ),
    "argument \"slope_col\" is missing, with no default"
  )
})

test_that("error slope_col cannot be found in slopes_df", {
  expect_error(
    flux_calc(
      slopes0,
      slope_col = "column_with_slope",
      conc_unit = "ppm",
      flux_unit = "mmol"
    ),
    "could not find slope_col in slopes_df"
  )
})

test_that("error some cols_keep do not exist", {
  expect_error(
    flux_calc(
      slopes0,
      slope_col = "f_slope",
      conc_unit = "ppm",
      flux_unit = "mmol",
      cols_keep = c("PAR", "site")
    ),
    "some names in cols_keep cannot be found in slopes_df"
  )
})

# reproducing the error I had in the example in the manuscript
# when there is a cut, flux_calc calculates a flux for keep and a flux for cut,
# which is wrong. I need to discard the cut data first.

test_that("calculating fluxes on dataset with cuts", {
  expect_snapshot(
    flux_calc(
      slopes30_flag,
      slope_col = "f_slope_corr",
      conc_unit = "ppm",
      flux_unit = "mmol",
      cut_col = "f_cut",
      keep_arg = "keep"
    ) |>
    dplyr::select(f_fluxID, flux, temp_air_avg, volume_setup)
  )
})

# testing having the chamber volume as a variable
test_that("volume can be a variable instead of a constant", {
  expect_snapshot(
    flux_calc(
      slopes0_vol,
      slope_col = "f_slope_tz",
      conc_unit = "ppm",
      flux_unit = "mmol",
      chamber_volume = "volume"
    ) |>
      dplyr::select(f_fluxID, flux, volume_setup, temp_air_avg)
  )
})

test_that("volume can be a variable instead of a constant (tube_volume)", {
  expect_snapshot(
    flux_calc(
      slopes0_vol_tube,
      slope_col = "f_slope_tz",
      conc_unit = "ppm",
      flux_unit = "mmol",
      chamber_volume = "volume",
      tube_volume = "tube_vol"
    ) |>
      dplyr::select(f_fluxID, flux, volume_setup, temp_air_avg)
  )
})

test_that("Fluxible workflow works from start to finish", {
  conc_test <- flux_match(
    co2_df_short,
    record_short
  )
  slopes_test <- suppressWarnings(flux_fitting(
    conc_test,
    fit_type = "exp"
  ))
  slopes_flag_test <- flux_quality(slopes_test)
  fluxes_test <- flux_calc(
    slopes_flag_test,
    slope_col = "f_slope_corr",
    conc_unit = "ppm",
    flux_unit = "mmol"
  )

  expect_snapshot(
    str(fluxes_test)
  )
})

test_that("flux_calc works with segmentation tool", {
  expect_snapshot(
    flux_calc(
      slopes_pftc7_flags,
      slope_col = "f_mean_slope_corr",
      conc_unit = "ppm",
      flux_unit = "mmol",
      temp_air_col = "temperature_c",
      atm_pressure = "pressure",
      chamber_volume = 2197,
      tube_volume = 0,
      plot_area = 1.69
    ) |>
      dplyr::select(
        f_fluxID, volume_setup, temp_air_avg, plot_area, flux, model
      )
  )
})

test_that("flux_calc with segmentation tool gives similar results as original data", {
  expected_calc <- flux_tent_output |>
    dplyr:: select(file_name, avg_temp, pressure, flux_value) |>
    dplyr::rename(
      f_fluxID = "file_name",
      temp_air_avg = "avg_temp",
      atm_pressure_avg = "pressure",
      flux = "flux_value"
    ) |>
    dplyr::mutate(
      f_fluxID = as.factor(f_fluxID),
      flux = flux * 3600 # to compare flux per hour instead of seconds
    ) |>
    dplyr::arrange(f_fluxID) |>
    data.frame()

  output_segment <-  pftc7_short |>
    group_by(file_name) |>
    dplyr::slice(8:n()) |>
    ungroup() |>
    flux_fitting(
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
    flux_quality(
      par_threshold = 650,
      sign_str_threshold = 95,
      pvalue_threshold = 100,
      rsquared_threshold = 0.71,
      sd_threshold = 0,
      ratio_threshold = 0
    ) |>
    flux_calc(
      slope_col = "f_mean_slope",
      conc_unit = "ppm",
      flux_unit = "micromol",
      temp_air_col = "temperature_c",
      atm_pressure = "pressure",
      chamber_volume = 2197,
      tube_volume = 0,
      plot_area = 1.69,
      cols_keep = c("f_quality_flag")
    ) |>
    dplyr::select(
      f_fluxID, temp_air_avg, atm_pressure_avg, flux
    ) |>
    dplyr::arrange(f_fluxID) |>
    data.frame()

  expect_equal(
    output_segment,
    expected_calc,
    tolerance = 0.02
  )
})
