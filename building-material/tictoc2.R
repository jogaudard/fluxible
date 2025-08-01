library(tidyverse)
library(fluxible)

conc_liahovden <- flux_match(
  raw_conc = co2_liahovden, # dataframe with raw gas concentration
  field_record = record_liahovden, # dataframe with meta data
  f_datetime = datetime, # column containing date and time
  start_col = start, # start date and time of each measurement
  measurement_length = 220, # length of measurements (in seconds)
  time_diff = 0 # time difference between f_datetime and start_col
)

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

flags_liahovden <- flux_quality(
  slopes_df = slopes_liahovden,
  f_conc = conc,
  ambient_conc = 421,
  error = 100,
  instr_error = 5
)

time_test <- function(nb_fluxes = c(1, 10, 30, 60, 100, 138),
 replicate, version, output) {
e_time <- c()
    
for (i in seq_along(nb_fluxes)) {

test_df <- flags_liahovden |>
  filter(
    f_fluxid %in% sample(c(1:138), nb_fluxes[i])
  )
time <- system.time(
    {
        test_df |>
flux_plot(
  f_conc = conc,
  f_datetime = datetime,
  f_ylim_upper = 600, # upper limit of y-axis
  f_ylim_lower = 350, # lower limit of x-axis
  y_text_position = 450, # position of text with flags and diagnostics
  output = output,
  f_plotname = "test"
)
})
e_time[i] <- time["elapsed"]
}
result <- tibble(
    nb_fluxes,
    e_time,
    replicate = rep(replicate, length(nb_fluxes)),
    fluxible = rep(paste(version, output, sep = "_"), length(nb_fluxes))
)

result
}

rep_1 <- time_test(
    replicate = 1,
    version = "v128",
    output = "pdfpages"
)

rep_2 <- time_test(
    replicate = 2,
    version = "v128",
    output = "pdfpages"
)

rep_3 <- time_test(
    replicate = 3,
    version = "v128",
    output = "pdfpages"
)

allreps <- bind_rows(
    rep_1,
    rep_2,
    rep_3
)

# check fluxible version!
saveRDS(allreps, "building-material/v128.rds")

v129 <- readRDS("building-material/v129.rds")
v128 <- readRDS("building-material/v128.rds")
v126 <- readRDS("building-material/v126.rds")

etime_all <- bind_rows(
  v129,
  v128,
  v126
)

etime_all |>
    ggplot(aes(nb_fluxes, e_time, color = fluxible)) +
    geom_point() +
    geom_smooth(se = FALSE)
