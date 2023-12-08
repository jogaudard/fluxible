test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# test on sample dataset

# need to redo the tests with now that I founf the issue with flux_length
# first we test just the fitting value
# when working on other variables, we want to check that the fitting value were not affected in the process
# slopes df labelled v1 are the datasets that were calculated with the most basic version of the fitting function and were checked graphically

test_that("fitting works with 0 second end cut",{
  ### setup
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")
  slopes0 <- readr::read_csv("data/slopes0v1.csv", col_types = "TddddffTTfddcdfddddddddddddT")

  ### test
  output <- flux_fitting_log(
      co2_conc
    )
  expect_equal(
    output$slope_tz,
    slopes0$slope_tz
  )
})

test_that("fitting works with 30 second end cut",{
  ### setup
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")
  slopes30 <- readr::read_csv("data/slopes30v1.csv", col_types = "TddddffTTfddcdfddddddddddddT")

  ### test
  expect_equal(
    flux_fitting_log(
      co2_conc,
      end_cut = 30
    ),
    slopes30
  )
})

test_that("fitting works with 60 second end cut",{
  ### setup
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")
  slopes60 <- readr::read_csv("data/slopes60v1.csv", col_types = "TddddffTTfddcdfddddddddddddT")

  ### test
  expect_equal(
    flux_fitting_log(
      co2_conc,
      end_cut = 60
    ),
    slopes60
  )
})

### need to test when data are missing
### tests on warnings

# test_that("warning that fluxIDs provided are not found in the dataset",{
#   ### setup
#   co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc")

#   ### test
 
# })
