# just a script to produce plots to include in the poster
# we can use the lia data used in the readme

conc_liahovden <- flux_match(co2_liahovden, record_liahovden)
slopes_exp_liahovden <- flux_fitting(conc_liahovden, fit_type = "exponential")
slopes_exp_liahovden <- flux_quality(slopes_exp_liahovden, fit_type = "expo", slope_col = "f_slope_tz")

slopes_exp_liahovden |>
  dplyr::filter(f_fluxID %in% c(28, 51, 100)) |> # we just show a sample of the plots to avoid slowing down the example
    mutate(
        f_fluxID = case_when(
            f_fluxID == 28 ~ "The Good",
            f_fluxID == 51 ~ "The Bad",
            f_fluxID == 100 ~ "The Ugly"
        ),
        f_fluxID = factor(f_fluxID, levels = c("The Good", "The Bad", "The Ugly"))
    ) |>
    flux_plot(
      fit_type = "exp",
      print_plot = TRUE,
      f_plotname = "example_lia_exp",
      f_ylim_lower = 375,
      f_ylim_upper = 525,
      f_ncol = 2,
      f_nrow = 2,
      y_text_position = 470,
      f_nudge_y = 0
      )
