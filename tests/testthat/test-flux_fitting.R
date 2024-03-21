test_that("works for exponential fitting", {
  qflux_fitting_exp <- purrr::quietly(flux_fitting_exp)
  qflux_fitting <- purrr::quietly(flux_fitting)
  expect_equal(
    qflux_fitting(co2_conc, fit_type = "expo"),
    qflux_fitting_exp(co2_conc)
  )
})

test_that("works for linear fitting", {
  qflux_fitting_lin <- purrr::quietly(flux_fitting_lin)
  qflux_fitting <- purrr::quietly(flux_fitting)

  expect_equal(
    qflux_fitting(co2_conc, fit_type = "lin"),
    qflux_fitting_lin(co2_conc)
  )
})

test_that("works for exponential fitting with cut", {
  qflux_fitting_exp <- purrr::quietly(flux_fitting_exp)
  qflux_fitting <- purrr::quietly(flux_fitting)

  expect_equal(
    qflux_fitting(co2_conc, fit_type = "expo", start_cut = 20),
    qflux_fitting_exp(co2_conc, start_cut = 20)
  )
})

test_that("works for linear fitting with cut", {
  qflux_fitting_lin <- purrr::quietly(flux_fitting_lin)
  qflux_fitting <- purrr::quietly(flux_fitting)


  expect_equal(
    qflux_fitting(co2_conc, fit_type = "lin", start_cut = 20),
    qflux_fitting_lin(co2_conc, start_cut = 20)
  )
})
