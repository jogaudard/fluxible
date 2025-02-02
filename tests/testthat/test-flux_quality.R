test_that("works for exponential fitting", {
  expect_snapshot(
    flux_quality(slopes0,
      f_conc,
      fit_type = "expo"
      # slope_col = "f_slope"
    ) |>
    dplyr::select(f_fluxID, f_quality_flag, f_RMSE, f_cor_coef) |>
    dplyr::distinct()
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_quality(slopes30lin,
    f_conc,
      fit_type = "li"
    ) |>
    dplyr::select(f_fluxID, f_quality_flag, f_pvalue, f_rsquared) |>
    dplyr::distinct()
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_quality(slopes30qua,
    f_conc,
      fit_type = "quadratic"
    ) |>
    dplyr::select(f_fluxID, f_quality_flag, f_pvalue, f_rsquared) |>
    dplyr::distinct()
  )
})
