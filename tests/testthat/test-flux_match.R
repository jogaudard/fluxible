# what do we need to test with match?
# standard sample of data
test_that("matching works", {
  ### setup
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT")
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc") %>%
     dplyr::arrange(datetime)


# test
  
  expect_equal(match_flux(co2_df_short, record_short), co2_conc)
})

# special case when flux is over midnight (change in date): this can be included in the standard match
# not enough data within the window provided returns a flag
# 

test_that("flags on nb of data", {
### setup
co2_df_missing <- readr::read_csv("data/co2_df_missing.csv", col_types = "Tdddd")
record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT")
co2_conc_missing <- readr::read_csv("data/co2_conc_missing.csv", col_types = "TddddffTTfddc") %>%
     dplyr::arrange(datetime)

### test

expect_equal(match_flux(co2_df_missing, record_short), co2_conc_missing)

})

# test that flags also get printed as warnings
test_that("warnings", {
  co2_df_missing <- readr::read_csv("data/co2_df_missing.csv", col_types = "Tdddd")
record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT")

expect_warning(match_flux(co2_df_missing, record_short),
"fluxID 1 : nb of data too low
 fluxID 3 : nb of data too low
 fluxID 6 : no data"
# "blop
# blip"
)
})

test_that("no warnings when no flags", {
  ### setup
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT")
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc") %>%
     dplyr::arrange(datetime)


# test
  
  expect_no_warning(match_flux(co2_df_short, record_short))
})

# test that the data type checking works (all the error messages)
