test_that("works for exponential fitting", {
  slopes0 <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    fit_type = "exponential"
  ))

  expect_snapshot(
    flux_quality(slopes0,
      conc
    ) |>
      dplyr::select(f_fluxid, f_quality_flag, f_RMSE, f_cor_coef,
                    f_ratio, f_gfactor) |>
      dplyr::distinct()
  )
})

test_that("works for linear fitting", {
  slopes30lin <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    end_cut = 30,
    fit_type = "linear"
  ))

  expect_snapshot(
    flux_quality(slopes30lin,
      conc
    ) |>
      dplyr::select(f_fluxid, f_quality_flag, f_pvalue, f_rsquared) |>
      dplyr::distinct()
  )
})

test_that("works for quadratic fitting", {
  slopes30qua <- suppressWarnings(flux_fitting(
    co2_conc,
    conc,
    datetime,
    end_cut = 30,
    fit_type = "quadratic"
  ))

  expect_snapshot(
    flux_quality(slopes30qua,
      conc
    ) |>
      dplyr::select(f_fluxid, f_quality_flag, f_pvalue,
                    f_rsquared, f_gfactor) |>
      dplyr::distinct()
  )
})
