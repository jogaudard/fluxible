# what do we need to test with match?
# standard sample of data
test_that("matching works", {
  ### setup
  co2_df_short <- readr::read_csv("data/co2_df_short.csv")
  record_short <- readr::read_csv("data/record_short.csv")
  co2_conc <- readr::read_csv("data/co2_conc.csv")

# test
  
  expect_equal(match_flux(co2_df_short, record_short), co2_conc)
})

# special case when flux is over midnight (change in date): this can be included in the standard match
# not enough data within the window provided returns a flag
# 

test_that("warning when not enough data", {
### setup
co2_df_missing <- readr::read_csv("data/co2_df_missing.csv")
record_short <- readr::read_csv("data/record_short.csv")
co2_conc_missing <- readr::read_csv("data/co2_conc_missing.csv")

### test

expect_equal(match_flux(co2_df_missing, record_short), co2_conc_missing)

})


# test that the data type checking works (all the error messages)
