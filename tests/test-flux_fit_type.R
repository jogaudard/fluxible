test_that("extracts the fit correclty", {
  expect_equal(
   flux_fit_type(slopes30qua),
   "quadratic"
  )
})


test_that("fit can be set by user", {
  expect_equal(
   flux_fit_type(slopes0, fit_type = "expo"),
   "exponential"
  )
})

test_that("fit transfer from flux_fitting", {
  expect_equal(
    slopes_test <- flux_fitting(co2_conc, fit_type = "exp")
   flux_fit_type(slopes_test),
   "exponential"
  )
})

test_that("fit_type can be NA", {
  expect_equal(
   flux_fit_type(slopes0),
   NA
  )
})

