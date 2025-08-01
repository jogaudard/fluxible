test_that("works for co2", {
  expect_snapshot(
    flux_drygas(pftc7_conc, co2, h2o)
  )
})

test_that("works for h2o", {
  expect_snapshot(
    flux_drygas(pftc7_conc, h2o, h2o)
  )
})
