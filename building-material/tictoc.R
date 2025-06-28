# several functions are too slow
# this script measure its running time
# we use the work flow from the fluxible vignette

library("profvis")
library(tictoc)
library(tidyverse)

conc_liahovden <- flux_match(
  raw_conc = co2_liahovden, # dataframe with raw gas concentration
  field_record = record_liahovden, # dataframe with meta data
  f_datetime = datetime, # column containing date and time
  start_col = start, # start date and time of each measurement
  measurement_length = 220, # length of measurements (in seconds)
  time_diff = 0 # time difference between f_datetime and start_col
)

tic("flux fitting zhao18")
slopes_liahovden <- flux_fitting(
  conc_df = conc_liahovden, # the output of flux_match
  f_conc = conc, # gas concentration column
  f_datetime = datetime, # date and time column
  f_start = f_start, # start of each measurement, provided by flux_match
  f_end = f_end, # end of each measurement, provided by flux_match
  f_fluxid = f_fluxid, # unique ID for each measurement, provided by flux_match
  fit_type = "exp_zhao18", # the model to fit to the gas concentration
  start_cut = 0, # seconds to prune at the start before fitting
  end_cut = 0 # seconds to prune at the end of all measurements before fitting
)
toc() # 10.845

flags_liahovden <- flux_quality(
  slopes_df = slopes_liahovden,
  f_conc = conc,
  ambient_conc = 421,
  error = 100,
  instr_error = 5
)

tic("flux_plot")
test <- flags_liahovden |>
  filter(
    f_fluxid %in% c(1:100)
  ) |>
  flux_plot(
    f_conc = conc,
    f_datetime = datetime,
    f_ylim_upper = 600, # upper limit of y-axis
    f_ylim_lower = 350, # lower limit of x-axis
    y_text_position = 450,
    f_plotname = "tst"
  )
toc() # 0.138 s

tic("print plot")
test
toc()

profvis({
test <- flags_liahovden |>
  filter(
    f_fluxid %in% c(1:100)
  ) |>
  flux_plot(
    f_conc = conc,
    f_datetime = datetime,
    f_ylim_upper = 600, # upper limit of y-axis
    f_ylim_lower = 350, # lower limit of x-axis
    y_text_position = 450,
    f_plotname = "tst"
    )
test
})

profvis({
flags_liahovden |>
  filter(
    f_fluxid %in% c(1:100)
  ) |>
  mutate(
    f_facetid = f_fluxid
  ) |>
  flux_plot_exp(
    f_conc = conc,
    f_datetime = datetime,
    y_text_position = 450,
    kappamax = NULL
    )
})

tic("flux_plot pdfpages")
flux_plot(
  flags_liahovden,
  f_conc = conc,
  f_datetime = datetime,
  f_ylim_upper = 600, # upper limit of y-axis
  f_ylim_lower = 350, # lower limit of x-axis
  y_text_position = 450, # position of text with flags and diagnostics
  facet_wrap_args = list( # facet_wrap arguments, if different than default
    nrow = 2,
    ncol = 2,
    scales = "free"
  ),
  output = "pdfpages"
)
toc() # 4.117 s

profvis({
  flux_plot(
  flags_liahovden,
  f_conc = conc,
  f_datetime = datetime,
  f_ylim_upper = 600, # upper limit of y-axis
  f_ylim_lower = 350, # lower limit of x-axis
  y_text_position = 450, # position of text with flags and diagnostics
  facet_wrap_args = list( # facet_wrap arguments, if different than default
    nrow = 2,
    ncol = 2,
    scales = "free"
  ),
  output = "pdfpages"
)
})
