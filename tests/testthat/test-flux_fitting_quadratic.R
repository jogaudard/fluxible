test_that("quadratic fit works", {
  expect_snapshot(
    flux_fitting(
      co2_conc,
      f_conc,
      f_datetime,
      f_start,
      f_end,
      f_fluxID,
      fit_type = "quadratic",
      t_zero = 10,
      end_cut = 30
    )
  )
})
