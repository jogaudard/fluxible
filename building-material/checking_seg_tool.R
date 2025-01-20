library(tidyverse)

pftc7_segmented_short_expected <- pftc7_segmented_short |>
    rename(
      f_conc = "co2_conc",
      f_datetime = "date_time",
      f_start = "start_time",
      f_fluxID = "file_name",
      f_rsquared = "f_rsq",
      f_adj_rsquared = "f_rsq_adj",
      f_pvalue = "f_pval"
    ) |>
    group_by(f_fluxID) |>
    mutate(
      f_cut = as.factor(f_cut),
      f_fluxID = as.factor(f_fluxID),
      f_slope = case_when(
        f_cut == "cut" ~ NA_real_,
        f_cut == "keep" ~ f_slope
      ),
      f_mean_slope_old = mean(f_slope, na.rm = TRUE)
    ) |>
    ungroup() |>
    arrange(f_datetime) |>
    select(f_fluxID, f_mean_slope_old) |>
        distinct() |>
    # select(f_datetime, f_fluxID, f_flag_fit, f_cut, ) |>
    #   group_by(f_fluxID) |>
    #   fill(f_mean_slope_old, .direction = "updown") |>
    #   ungroup() |>
    data.frame()

    pftc7_short_segmented_test <- pftc7_short |>
    group_by(file_name) |>
    dplyr::slice(8:n()) |>
    ungroup() |>
    flux_fitting(
      # pftc7_short,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
    flux_quality(par_threshold = 650,
  sign_str_threshold = 95,
  pvalue_threshold = 0.05,
  rsquared_threshold = 0.7,
  sd_threshold = 1,
  ratio_threshold = 0) |>
    arrange(f_datetime) |>
    select(f_fluxID, f_mean_slope) |>
        distinct() |>
    data.frame()


comparison <- full_join(pftc7_segmented_short_expected, pftc7_short_segmented_test, by = "f_fluxID")

comparison |>
ggplot(aes(f_mean_slope_old, f_mean_slope)) +
geom_point() +
ylim(-0.1, 0.2) +
xlim(-0.1, 0.2) +
geom_smooth(method = "lm") +
geom_abline(slope = 1, intercept = 0)

test <- pftc7_short |>
    group_by(file_name) |>
    dplyr::slice(8:n()) |>
    ungroup() |>
    flux_fitting(
      # pftc7_short,
      fit_type = "segments",
      start_col = "start_time",
      end_col = "f_end",
      start_cut = 0,
      end_cut = 0,
      conc_col = "co2_conc",
      par_col = "par",
      datetime_col = "date_time",
      h2o_col = "h2o_conc",
      sign_str_col = "signal_strength",
      fluxid_col = "file_name",
      h2o_correction = TRUE,
      min_seg_length = 30
    ) |>
    flux_quality(par_threshold = 650,
  sign_str_threshold = 95,
  pvalue_threshold = 0,
  rsquared_threshold = 0,
  sd_threshold = 1,
  ratio_threshold = 0) |>
  select(!corrected_for_water_vapor) |>
    arrange(f_datetime) |>
    data.frame()

   expected <- pftc7_segmented_short |>
    rename(
      f_conc = "co2_conc",
      f_datetime = "date_time",
      f_start = "start_time",
      f_fluxID = "file_name",
      f_rsquared = "f_rsq",
      f_adj_rsquared = "f_rsq_adj",
      f_pvalue = "f_pval"
    ) |>
    group_by(f_fluxID) |>
    mutate(
      f_cut = as.factor(f_cut),
      f_fluxID = as.factor(f_fluxID),
      f_end = f_start + 119,
      f_cut = case_when(
        f_datetime > f_end ~ "cut",
        f_datetime <= f_end ~ f_cut
      ),
      f_slope = case_when(
        f_cut == "cut" ~ NA_real_,
        f_cut == "keep" ~ f_slope
      ),
      f_mean_slope_old = mean(f_slope)
    ) |>
    ungroup() |>
    arrange(f_datetime) |>
    # select(f_datetime, f_fluxID, f_flag_fit, f_cut, ) |>
    #   group_by(f_fluxID) |>
    #   fill(f_mean_slope_old, .direction = "updown") |>
    #   ungroup() |>
    data.frame()

full_comparison <- full_join(test, expected)
View(full_comparison)
View(test)
View(expected)
