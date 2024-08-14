# just a script to produce plots to include in the poster
# we can use the lia data used in the readme

conc_liahovden <- flux_match(co2_liahovden, record_liahovden)
slopes_exp_liahovden <- flux_fitting(conc_liahovden, fit_type = "exponential")
slopes_exp_liahovden <- flux_quality(slopes_exp_liahovden, fit_type = "expo", slope_col = "f_slope_tz")

# special function to make the plots for the poster (bigger and co)
flux_plot_exp_poster <- function(slopes_df,
                          datetime_col = "f_datetime",
                          conc_col = "f_conc",
                          cut_col = "f_cut",
                          fit_col = "f_fit",
                          fit_slope_col = "f_fit_slope",
                          quality_flag_col = "f_quality_flag",
                          fluxID_col = "f_fluxID",
                          start_col = "f_start",
                          b_col = "f_b",
                          cor_coef_col = "f_cor_coef",
                          RMSE_col = "f_RMSE",
                          color_discard = "#D55E00",
                          color_cut = "#D55E00",
                          color_keep = "#009E73",
                          color_ok = "#000000",
                          color_zero = "#CC79A7",
                          f_date_breaks = "1 min",
                          f_minor_breaks = "10 sec",
                          f_date_labels = "%e/%m \n %H:%M",
                          f_ylim_upper = 800,
                          f_ylim_lower = 400,
                          f_scales = "free",
                          f_plotname = "plot_quality_exp",
                          f_ncol = 4,
                          f_nrow = 3,
                          y_text_position = 500,
                          f_nudge_y = 100,
                          print_plot = "FALSE") {
  f_scales <- match.arg(f_scales, c("free", "fixed"))
  f_plotname <- paste("f_quality_plots/", f_plotname, ".pdf", sep = "")

  folder <- "./f_quality_plots"
  if (!file.exists(folder)) {
    dir.create(folder)
  }



  slopes_df <- slopes_df |>
    rename(
      f_datetime = all_of((datetime_col)),
      f_conc = all_of((conc_col)),
      f_cut = all_of((cut_col)),
      f_fit = all_of((fit_col)),
      f_fit_slope = all_of(((fit_slope_col))),
      f_quality_flag = all_of((quality_flag_col)),
      f_fluxID = all_of((fluxID_col)),
      f_start = all_of(((start_col))),
      f_b = all_of((b_col)),
      f_cor_coef = all_of(((cor_coef_col))),
      f_RMSE = all_of(((RMSE_col)))
    )

  param_df <- slopes_df |>
    select(
      "f_conc", "f_start", "f_fluxID", "f_RMSE", "f_cor_coef", "f_b", "f_cut",
      "f_quality_flag"
    ) |>
    filter(.data$f_cut == "keep") |>
    group_by(.data$f_fluxID) |>
    mutate(
      conc_start = .data$f_conc[1]
    ) |>
    ungroup() |>
    select(!"f_conc") |>
    distinct() |>
    mutate(
      f_RMSE = round(.data$f_RMSE, digits = 1),
      f_cor_coef = round(.data$f_cor_coef, digits = 2),
      f_b = round(.data$f_b, digits = 5),
      print_col = paste(
        .data$f_quality_flag, "\n",
        "RMSE = ", .data$f_RMSE, "\n", "Corr coef = ",
        .data$f_cor_coef, "\n", "b = ", .data$f_b,
        sep = ""
      )
    )

  plot_exp <- slopes_df |>
    theme_classic() +
    ggplot(aes(x = .data$f_datetime)) +
    # geom_point(
    #   aes(y = .data$f_conc, color = .data$f_cut),
    #   size = 0.8
    # ) +
    # geom_line(
    #   aes(y = .data$f_fit, color = .data$f_quality_flag),
    #   linetype = "longdash", linewidth = 0.8
    # ) +
    # geom_line(
    #   aes(y = .data$f_fit_slope, color = .data$f_quality_flag),
    #   linetype = "dashed", linewidth = 0.8
    # ) +
    # scale_color_manual(values = c(
    #  "keep" = ((color_keep)),
    #   "cut" = ((color_cut)),
    #   "ok" = ((color_ok)),
    #   "discard" = ((color_discard)),
    #   "zero" = ((color_zero)),
    #   "start_error" = ((color_discard)),
    #   "weird_flux" = ((color_discard))
    # )) +
    # scale_x_datetime(
    #   date_breaks = ((f_date_breaks)), minor_breaks = ((f_minor_breaks)),
    #   date_labels = ((f_date_labels))
    # ) +
    # ylim(((f_ylim_lower)), ((f_ylim_upper))) +
    # geom_text(
    #   data = param_df,
    #   aes(x = .data$f_start, y = ((y_text_position)), label = .data$print_col),
    #   vjust = 0, hjust = "inward", nudge_y = ((f_nudge_y))
    # ) +
    #   facet_wrap(~f_fluxID, scales = ((f_scales))) +
    # facet_wrap_paginate(
    #   ~f_fluxID,
    #   ncol = ((f_ncol)), nrow = ((f_nrow)), scales = ((f_scales))
    # ) +
    # labs(
    #   title = "Fluxes quality assessment",
    #   x = "Datetime",
    #   y = "Concentration",
    #   colour = "Quality flags"
    # )









  pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
  for (i in 1:n_pages(plot_exp)) {
    print(plot_exp +
      facet_wrap_paginate(~f_fluxID,
        ncol = ((f_ncol)), nrow = ((f_nrow)),
        page = i, scales = ((f_scales))
      ))
  }
  quietly(dev.off())

  print("Saving plots in f_quality_plots folder.")

  if (((print_plot)) == TRUE) {
    return(plot_exp)
  }
}



slopes_exp_liahovden |>
  dplyr::filter(f_fluxID %in% c(28, 51, 100)) |> # we just show a sample of the plots to avoid slowing down the example
    # mutate(
    #     f_fluxID = case_when(
    #         f_fluxID == 28 ~ "The Good",
    #         f_fluxID == 51 ~ "The Bad",
    #         f_fluxID == 100 ~ "The Ugly"
    #     ),
    #     f_fluxID = factor(f_fluxID, levels = c("The Good", "The Bad", "The Ugly"))
    # ) |>
    # view()
    flux_plot(
      fit_type = "exp",
      print_plot = TRUE,
      f_plotname = "poster_plot",
      f_ylim_lower = 375,
      f_ylim_upper = 525,
      f_ncol = 2,
      f_nrow = 2,
      y_text_position = 470,
      f_nudge_y = 0
      )
