test_that("flux calculation is correct", {
  output <- flux_calc(slopes0, slope_col = "f_slope_tz")

  expect_equal(
    output$flux,
    co2_fluxes$flux,
    tolerance = 0.001
  )
})


test_that("averaging works", {
  output <- flux_calc(
    slopes0,
    slope_col = "f_slope_tz",
    cols_ave = c("PAR", "temp_soil")
  )


  expect_snapshot(output)
})

test_that("keeping works", {
  expect_snapshot(flux_calc(
    slopes0,
    slope_col = "f_slope_tz",
    cols_keep = c("turfID", "type", "f_start")
  ))
})

test_that("keeping and averaging work together", {
  expect_snapshot(flux_calc(
    slopes0,
    slope_col = "f_slope_tz",
    cols_keep = c("turfID", "type", "f_start"),
    cols_ave = c("PAR", "temp_soil")
  ))
})

test_that("fahrenheit conversion works", {
  expect_snapshot(flux_calc(
    slopes0_temp,
    slope_col = "f_slope_tz",
    temp_air_col = "temp_fahr",
    temp_air_unit = "fahrenheit"
  ))
})

test_that("kelvin conversion works", {
  expect_snapshot(flux_calc(
    slopes0_temp,
    slope_col = "f_slope_tz",
    temp_air_col = "temp_kelvin",
    temp_air_unit = "kelvin"
  ))
})



test_that("errors on arguments types", {
  expect_error(
    flux_calc(
      slopes0,
      slope_col = "slope_tz",
      chamber_volume = "sort of big"
    ),
    "chamber_volume has to be a double"
  )
})

test_that("error on air temp units", {
  expect_error(
    flux_calc(
      slopes0,
      slope_col = "slope_tz",
      temp_air_unit = "melvin"
    ),
    "temp_air_unit has to be either celsius, fahrenheit or kelvin"
  )
  # Played for Laughs in the 1985 humor book "Science Made Stupid" by Tom Weller,
  # in which a table in its appendix lists such units as the "arg"
  # (the unit of work done incorrectly), the "galumph" (unit of waste motion),
  # the "lumpen" (unit of resistance to getting out of bed in the morning),
  # and the "melvin" (unit of temperature, "as measured from absolutely perfect
  # to absolutely awful"). A separate table of conversions for weights
  # and measures on the same page listed equivalencies such as
  # "325 cubebs = 1 furbish; 6 furbishes = 1 nautical smile;
  # 20 nautical smiles = 1 minor league; 3 minor leagues = 1 major league"
  # and "24 carrots = 1 pickelweight; 30 pickelweights = 1 tuna;
  # 1000 tuna = 1 short ton; 1.37 short tons = 1 tall ton".
  # (https://allthetropes.org/wiki/Fantastic_Measurement_System)
})

test_that("error that slope column is missing", {
  expect_error(
    flux_calc(
      slopes0
    ),
    "argument \"slope_col\" is missing, with no default"
  )
})

test_that("error slope_col cannot be found in slope_df", {
  expect_error(
    flux_calc(
      slopes0,
      slope_col = "column_with_slope"
    ),
    "could not find slope_col in slope_df"
  )
})

test_that("error some cols_keep do not exist", {
  expect_error(
    flux_calc(
      slopes0,
      slope_col = "f_slope_tz",
      cols_keep = c("PAR", "site")
    ),
    "some names in cols_keep cannot be found in slope_df"
  )
})

# reproducing the error I had in the example in the manuscript
# when there is a cut, flux_calc calculates a flux for keep and a flux for cut, which is wrong. I need to discard the cut data first.

test_that("calculating fluxes on dataset with cuts filters out the cuts first", {
  expect_snapshot(
    flux_calc(
  slopes30_flag,
  slope_col = "f_slope_corr",
  cut_col = "f_cut",
  keep_filter = "keep"
  )
  )
})
