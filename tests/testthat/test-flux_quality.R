test_that("works for exponential fitting", {
  expect_snapshot(
    flux_quality(slopes0,
      f_conc,
      fit_type = "expo"
      # slope_col = "f_slope"
    )
  )
})

test_that("works for linear fitting", {
  expect_snapshot(
    flux_quality(slopes30lin,
    f_conc,
      fit_type = "li"
    )
  )
})

test_that("works for quadratic fitting", {
  expect_snapshot(
    flux_quality(slopes30qua,
      fit_type = "quadratic"
    )
  )
})
