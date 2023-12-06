test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# test on sample dataset

test_that("fitting works with 0 second end cut",{
  ### setup
  co2_conc <- readr::read_csv("data/co2_conc.csv")
  slopes0 <- readr::read_csv("data/slopes0.csv")

  ### test
  expect_equal(
    flux_fitting_log(
      co2_conc
    ),
    slopes0
  )
})

test_that("fitting works with 30 second end cut",{
  ### setup
  co2_conc <- readr::read_csv("data/co2_conc.csv")
  slopes30 <- readr::read_csv("data/slopes30.csv")

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
  co2_conc <- readr::read_csv("data/co2_conc.csv")
  slopes30 <- readr::read_csv("data/slopes60.csv")

  ### test
  expect_equal(
    flux_fitting_log(
      co2_conc,
      end_cut = 60
    ),
    slopes60
  )
})