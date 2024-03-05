test_that("works for exponential fitting", {

#   qflux_fitting_exp <- purrr::quietly(flux_quality_exp) # to make the warnings quiet
#   qflux_fitting <- purrr::quietly(flux_fitting) # to make the warnings quiet

  expect_equal(
    flux_quality(slopes0, fit_type = "expo", fluxID_col = "fluxID", conc_col = "conc", b_col = "b", time_col = "time", fit_col = "fit", slope_col = "slope_tz"),
    flux_quality_exp(slopes0, fluxID_col = "fluxID", conc_col = "conc", b_col = "b", time_col = "time", fit_col = "fit", slope_col = "slope_tz")
  )
})

test_that("works for linear fitting", {

#   qflux_fitting_lin <- purrr::quietly(flux_fitting_lin) # to make the warnings quiet
#   qflux_fitting <- purrr::quietly(flux_fitting) # to make the warnings quiet


  expect_equal(
    flux_quality(slopes0lin, fit_type = "li", fluxID_col = "fluxID", slope_col = "slope"),
    flux_quality_lin(slopes0lin, fluxID_col = "fluxID", slope_col = "slope")
  )
})