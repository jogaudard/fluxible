test_that("fitting works with 0 second end cut",{
  ### setup
  # co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")
  # slopes0lin <- readr::read_csv("data/slopes0lin.csv", col_types = "TddddffTTfddcdfddddddddddddT")

  ### test

    qflux_fitting_lin <- purrr::quietly(flux_fitting_lin) # to make the warnings quiet

  fitting_lin_call <- qflux_fitting_lin(
      co2_conc
    )

  output <- fitting_lin_call$result

  expect_equal(
    output$slope,
    slopes0lin$slope
  )
})

test_that("fitting works with 30 second end cut",{
  ### setup
  # co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")
  # slopes30lin <- readr::read_csv("data/slopes30lin.csv", col_types = "TddddffTTfddcdfddddddddddddT")

  ### test
  output <- flux_fitting_lin(
      co2_conc,
      end_cut = 30
    )
  expect_equal(
    output$slope,
    slopes30lin$slope
  )
})

test_that("fitting works with 60 second end cut",{
  ### setup
  # co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")
  # slopes60lin <- readr::read_csv("data/slopes60lin.csv", col_types = "TddddffTTfddcdfddddddddddddT")

  ### test
  output <- flux_fitting_lin(
      co2_conc,
      end_cut = 60
    )
  expect_equal(
    output$slope,
    slopes60lin$slope
  )
})

### need to test when data are missing
# warning that NAs were dropped in some fluxID

test_that("warnings when NAs are dropped in conc", {
#  co2_conc_missing <- readr::read_csv("data/co2_conc_missing.csv", col_types = "TddddffTTfddc")

expect_warning(
  flux_fitting_lin(co2_conc_missing),
" fluxID 1 : slope was estimated on 70 points out of 210 seconds because data are missing
 fluxID 2 : slope was estimated on 121 points out of 210 seconds because data are missing
 fluxID 3 : slope was estimated on 102 points out of 210 seconds because data are missing
 fluxID 5 : slope was estimated on 161 points out of 210 seconds because data are missing"
)


})

# warning when there is no data in some fluxID

test_that("warnings when thre is no data in conc", {
#  co2_conc_missing <- readr::read_csv("data/co2_conc_missing.csv", col_types = "TddddffTTfddc")

expect_warning(
  flux_fitting_lin(co2_conc_missing),
" fluxID 6 : slope could not be estimated because there are no data in the conc column"
)

})

test_that("warnings with cutting", {
#  co2_conc_missing <- readr::read_csv("data/co2_conc_missing.csv", col_types = "TddddffTTfddc")

expect_warning(
  flux_fitting_lin(
    co2_conc_missing,
    start_cut = 10
    ),
" fluxID 1 : slope was estimated on 70 points out of 200 seconds because data are missing
 fluxID 2 : slope was estimated on 121 points out of 200 seconds because data are missing
 fluxID 3 : slope was estimated on 102 points out of 200 seconds because data are missing
 fluxID 5 : slope was estimated on 151 points out of 200 seconds because data are missing"
)


})

test_that("error on arguments", {
#  co2_conc_missing <- readr::read_csv("data/co2_conc_missing.csv", col_types = "TddddffTTfddc")

expect_error(
  flux_fitting_lin(
    co2_conc_missing,
    start_cut = "Voldemort"
    ),
"start_cut has to be a double"
)


})



test_that("cutting too much", {
#  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")

expect_error(
  flux_fitting_lin(
    co2_conc,
    start_cut = 120,
    end_cut = 100
    ),
"You cannot cut more than the length of the measurements! ((start_cut + end_cut) >= length_flux_max)",
fixed = TRUE # need that because there parenthesis in the error message
)


})

test_that("renaming works", {
#  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")


  co2_conc_names <- co2_conc %>%
     dplyr::rename(
      date_time = datetime,
      finish = end,
      co2 = conc
     )

  qflux_fitting_lin <- purrr::quietly(flux_fitting_lin) # to make the warnings quiet


expect_no_error(
  qflux_fitting_lin(
    co2_conc_names,
    datetime_col = "date_time",
    end_col = "finish",
    conc_col = "co2"
  )
)


})