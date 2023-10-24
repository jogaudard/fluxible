# what do we need to test with match?
# standard sample of data
test_that("matching works", {
  ### setup
  co2_df_short <- read_csv("data/co2_df_short.csv")
  record_short <- read_csv("data/co2_df_short.csv")
  co2_conc <- read_csv("data/co2_conc.csv")


  
  expect_equal(match.flux(co2_df_short, record_short), co2_conc)
})

# special case when flux is over midnight (change in date): this can be included in the standard match
# not enough data within the window provided returns a flag
# 


