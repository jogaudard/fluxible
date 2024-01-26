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



# test_that("errors on arguments types", {
#   expect_equal()
# })

# test_that("error on air temp units", {
#   expect_equal()
# })