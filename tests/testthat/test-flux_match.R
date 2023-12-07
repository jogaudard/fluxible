# what do we need to test with match?
# standard sample of data
test_that("matching works", {
  ### setup
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc", na = c("#N/A", "NA")) %>%
     dplyr::arrange(datetime)


# test
  
  expect_equal(match_flux(
    co2_df_short,
    record_short
    ), co2_conc)
})

test_that("time_diff works", {
  ### setup
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc", na = c("#N/A", "NA")) %>%
     dplyr::arrange(datetime)

co2_df_short <- co2_df_short %>%
   dplyr::mutate(
    datetime = datetime - 180 # logger is lagging 3 minutes behind
   )

# test
  
  expect_equal(match_flux(
    co2_df_short,
    record_short,
    time_diff = 180
    ), co2_conc)
})

test_that("renaming variables works", {
  ### setup
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc", na = c("#N/A", "NA")) %>%
     dplyr::arrange(datetime)

  co2_df_short <- co2_df_short %>%
     dplyr::rename(
      CO2_conc = conc,
      date_time = datetime
     )

  record_short <- record_short %>%
     dplyr::rename(
      starting = start
     )

# we do not need to adapt to the names of the user?
  # co2_conc <- co2_conc %>%
  #    dplyr::rename(
  #     CO2_conc = CO2,
  #     date_time = datetime,
  #     starting = start
  #    )

# test
  
  expect_equal(
    match_flux(
      co2_df_short,
      record_short,
      datetime_col = "date_time",
      conc_col = "CO2_conc",
      start_col = "starting"
      ), co2_conc)
})

# special case when flux is over midnight (change in date): this can be included in the standard match
# not enough data within the window provided returns a flag
# 

test_that("flags on nb of data", {
### setup
co2_df_missing <- readr::read_csv("data/co2_df_missing.csv", col_types = "Tdddd", na = "#N/A")
record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
co2_conc_missing <- readr::read_csv("data/co2_conc_missing.csv", col_types = "TddddffTTfddc", na = c("#N/A", "NA")) %>%
     dplyr::arrange(datetime)

### test


expect_equal(
  suppressWarnings( # warnings are expected, they are tested in another test
  match_flux(
    co2_df_missing,
  record_short
  )),
   co2_conc_missing)




})

# test that flags also get printed as warnings
test_that("warnings", {
  co2_df_missing <- readr::read_csv("data/co2_df_missing.csv", col_types = "Tdddd", na = "#N/A")
record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")

expect_warning(match_flux(
  co2_df_missing,
  record_short
  ),
"fluxID 1 : nb of data too low
 fluxID 3 : nb of data too low
 fluxID 6 : no data"

)
})

test_that("no warnings when no flags", {
  ### setup
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  co2_conc <- readr::read_csv("data/co2_conc.csv", col_types = "TddddffTTfddc", na = "#N/A") %>%
     dplyr::arrange(datetime)


# test
  
  expect_no_warning(match_flux(
    co2_df_short,
    record_short
    ))
})

# test that the data type checking works (all the error messages)

test_that("error on datetime", {
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  

  co2_df_short <- co2_df_short %>%
     dplyr::mutate(
      datetime = lubridate::date(datetime)
     )

  expect_error(
    match_flux(
      co2_df_short,
      record_short
      ),
    "datetime in raw_conc dataframe is not ymd_hms!"
  )
})

test_that("error on conc variable", {
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  

  co2_df_short <- co2_df_short %>%
     dplyr::mutate(
      conc = as.character(conc)
     )

  expect_error(
    match_flux(
      co2_df_short,
      record_short),
    "conc is not a double"
  )
})

test_that("error on start", {
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  

  record_short <- record_short %>%
     dplyr::mutate(
      start = lubridate::hour(start)
     )

  expect_error(
    match_flux(
      co2_df_short,
      record_short
      ),
    "start in field_record dataframe is not ymd_hms!"
  )
})

test_that("error on startcrop", {
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  


  expect_error(
    match_flux(co2_df_short,
    record_short,
    startcrop = "blip"),
    "startcrop has to be a double"
  )
})

test_that("error on measurement_length", {
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  


  expect_error(
    match_flux(
      co2_df_short,
      record_short,
      measurement_length = "blip"
      ),
    "measurement_length has to be a double"
  )
})

test_that("error on ratio_threshold", {
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  


  expect_error(
    match_flux(
      co2_df_short,
      record_short,
      ratio_threshold = 2
      ),
    "ratio_threshold has to be a number between 0 and 1"
  )
})

test_that("error on time_diff", {
  co2_df_short <- readr::read_csv("data/co2_df_short.csv", col_types = "Tdddd", na = "#N/A")
  record_short <- readr::read_csv("data/record_short.csv", col_types = "ffT", na = "#N/A")
  


  expect_error(
    match_flux(
      co2_df_short,
      record_short,
      time_diff = "comment est votre blanquette?"
      ),
    "time_diff has to be a double"
  )
})