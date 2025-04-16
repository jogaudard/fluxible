library(fluxible)
library(tidyverse)

conc_liahovden <- flux_match(
  co2_liahovden,
  record_liahovden,
  datetime,
  start,
  conc,
  startcrop = 0,
  measurement_length = 180,
  ratio_threshold = 0.5,
  time_diff = 0
)

slopes_exp_liahovden <- flux_fitting(
  conc_liahovden,
  conc,
  datetime,
  end_cut = 0,
  fit_type = "exp_zhao18"
)

slopes_exp_liahovden_flag <- flux_quality(
  slopes_exp_liahovden,
  conc,
  f_rsquared = f_rsquared_lm,
  f_pvalue = f_pvalue_lm,
  error = 50
)

slopes_exp_liahovden_flag |>
filter(
    f_fluxid %in% c(54, 49)
) |>
flux_plot(
    conc,
    datetime,
    output = "pdfpages",
    # ggsave_args = list(
    #     width = 300,
    #     height = 100,
    #     units = "px"
    # ),
    f_plotname = "plot_EGU25",
    f_ylim_lower = 350,
    f_ylim_upper = 475
    )
