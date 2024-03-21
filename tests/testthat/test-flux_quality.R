test_that("works for exponential fitting", {
  expect_equal(
    flux_quality(slopes0,
      fit_type = "expo", fluxID_col = "fluxID",
      conc_col = "conc", b_col = "b", time_col = "time",
      fit_col = "fit", slope_col = "slope_tz", cut_col = "cut"
    ),
    flux_quality_exp(slopes0,
      fluxID_col = "fluxID", conc_col = "conc",
      b_col = "b", time_col = "time", fit_col = "fit",
      slope_col = "slope_tz", cut_col = "cut"
    )
  )
})

test_that("works for linear fitting", {
  expect_equal(
    flux_quality(slopes0lin,
      fit_type = "li", fluxID_col = "fluxID",
      slope_col = "slope", conc_col = "conc"
    ),
    flux_quality_lin(slopes0lin,
      fluxID_col = "fluxID",
      slope_col = "slope", conc_col = "conc"
    )
  )
})
