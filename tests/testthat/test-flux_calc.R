test_that("flux calculation is correct", {
    co2_fluxes <- readr::read_csv("data/fluxes.csv")
    slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddT")

    output <- flux_calc(slopes0, slope_col = "slope_tz")

  expect_equal(
    output$flux,
    co2_fluxes$flux,
    tolerance = 0.001 #took 3 decimals for air temperature when manually calculating fluxes for the test
  )
})


test_that("averaging works", {
    co2_fluxes <- readr::read_csv("data/fluxes.csv", col_types = "fdddddffT")
    slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddT")

    output <- flux_calc(
        slopes0,
        slope_col = "slope_tz",
        cols_ave = c("PAR", "temp_soil")
        )

    output <- dplyr::select(output, PAR, temp_soil, temp_air_ave)

    expected <- dplyr::select(co2_fluxes, PAR, temp_soil, temp_air_ave)
  expect_equal(
    output,
    expected,
    tolerance = 0.001 #took 3 decimals when manually calculating fluxes for the test
  )
})

test_that("keeping works", {
    co2_fluxes <- readr::read_csv("data/fluxes.csv", col_types = "fdddddffT")
    slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddT")


    output <- flux_calc(
        slopes0,
        slope_col = "slope_tz",
        cols_keep = c("turfID", "type", "start")
        )

    output <- dplyr::select(output, turfID, type, start)

    expected <- dplyr::select(co2_fluxes, turfID, type, start)
  expect_equal(
    output,
    expected,
    tolerance = 0.001 #took 3 decimals when manually calculating fluxes for the test
  )

})

test_that("keeping and averaging work together", {
    co2_fluxes <- readr::read_csv("data/fluxes.csv", col_types = "fdddddffT")
    slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddT")


    output <- flux_calc(
        slopes0,
        slope_col = "slope_tz",
        cols_keep = c("turfID", "type", "start"),
        cols_ave = c("PAR", "temp_soil")
        )

    output <- dplyr::select(output, turfID, type, start, PAR, temp_soil, temp_air_ave)

    expected <- dplyr::select(co2_fluxes, turfID, type, start, PAR, temp_soil, temp_air_ave)
  expect_equal(
    output,
    expected,
    tolerance = 0.001 #took 3 decimals when manually calculating fluxes for the test
  )

})

test_that("fahrenheit conversion works", {
co2_fluxes <- readr::read_csv("data/fluxes.csv", col_types = "fdddddffTdd")
    slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddTd")

output <- flux_calc(
  slopes0,
  slope_col = "slope_tz",
  temp_air_col = "temp_fahr",
  temp_air_unit = "fahrenheit"
)

output <- dplyr::select(output, fluxID, flux, temp_air_ave)
output <- dplyr::rename(
  output,
  temp_fahr = temp_air_ave
)
expected <- dplyr:: select(co2_fluxes, fluxID, flux, temp_fahr)

expect_equal(
    output,
    expected,
    tolerance = 0.001 #took 3 decimals when manually calculating fluxes for the test
  )

    
  
})

test_that("kelvin conversion works", {
  co2_fluxes <- readr::read_csv("data/fluxes.csv", col_types = "fdddddffTdd")
    slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddTdd")

output <- flux_calc(
  slopes0,
  slope_col = "slope_tz",
  temp_air_col = "temp_kelvin",
  temp_air_unit = "kelvin"
)

output <- dplyr::select(output, fluxID, flux, temp_air_ave)
output <- dplyr::rename(
  output,
  temp_kelvin = temp_air_ave
)
expected <- dplyr:: select(co2_fluxes, fluxID, flux, temp_kelvin)

expect_equal(
    output,
    expected,
    tolerance = 0.001 #took 3 decimals when manually calculating fluxes for the test
  )
})



test_that("errors on arguments types", {
      slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddTdd")

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
  slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddTdd")

  expect_error(
    flux_calc(
      slopes0,
      slope_col = "slope_tz",
      temp_air_unit = "melvin"
    ),
    "temp_air_unit has to be either celsius, fahrenheit or kelvin"
  )
  # Played for Laughs in the 1985 humor book "Science Made Stupid" by Tom Weller, in which a table in its appendix lists such units as the "arg" (the unit of work done incorrectly), the "galumph" (unit of waste motion), the "lumpen" (unit of resistance to getting out of bed in the morning), and the "melvin" (unit of temperature, "as measured from absolutely perfect to absolutely awful"). A separate table of conversions for weights and measures on the same page listed equivalencies such as "325 cubebs = 1 furbish; 6 furbishes = 1 nautical smile; 20 nautical smiles = 1 minor league; 3 minor leagues = 1 major league" and "24 carrots = 1 pickelweight; 30 pickelweights = 1 tuna; 1000 tuna = 1 short ton; 1.37 short tons = 1 tall ton". (https://allthetropes.org/wiki/Fantastic_Measurement_System)
})

test_that("error that slope column is missing", {
  slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddTdd")

  expect_error(
    flux_calc(
      slopes0
    ),
    "argument \"slope_col\" is missing, with no default"
  )
})

test_that("error slope_col cannot be found in slope_df", {
  slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddTdd")

  expect_error(
    flux_calc(
      slopes0,
      slope_col = "column_with_slope"
    ),
    "could not find slope_col in slope_df"
  )
})

test_that("error some cols_keep do not exist", {
  slopes0 <- readr::read_csv("data/slopes0.csv", col_types = "TddddffTTfddcdfddddddddddddTdd")

  expect_error(
    flux_calc(
      slopes0,
      slope_col = "slope_tz",
      cols_keep = c("PAR", "site")
    ),
    "some names in cols_keep cannot be found in slope_df"
  )
})