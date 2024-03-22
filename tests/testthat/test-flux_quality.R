test_that("works for exponential fitting", {
  expect_equal(
    flux_quality(slopes0,
      fit_type = "expo", slope_col = "f_slope_tz"
    ),
    flux_quality_exp(slopes0,
      slope_col = "f_slope_tz"
    )
  )
})

test_that("works for linear fitting", {
  expect_equal(
    flux_quality(slopes0lin,
      fit_type = "li"
    ),
    flux_quality_lin(slopes0lin
    )
  )
})
