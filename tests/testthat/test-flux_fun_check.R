test_that("gets error with wrong inputs", {
  expect_error(
    flux_fun_check(
      slopes0,
      col_numeric = c("f_datetime", "temp_soil"),
      col_datetime = c("f_start", "f_fluxID")
    ),
    " f_datetime has to be numeric
 f_fluxID has to be POSIXct"
  )
})
