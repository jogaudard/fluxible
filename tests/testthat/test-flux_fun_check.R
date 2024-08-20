test_that("gets error with wrong inputs", {
    number <- 3
    not_a_number <- "this is definitely not a number"
    expect_error(
        flux_fun_check(
            slopes0,
            col_numeric = c("f_datetime", "temp_soil"),
            col_time = c("f_start", "f_fluxID"),
            arg_numeric = c("number", "not_a_number")
            # I know this is wrong, it is just to test the error message
        ),
        " f_datetime has to be numeric
 f_fluxID has to be POSIXct
 not_a_number has to be numeric"
    )

})
