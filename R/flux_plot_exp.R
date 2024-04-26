#' ploting fluxes for fit evaluation
#' @description plots the fluxes and indicates what should be
#' discarded or replaced by zero
#' @param slopes_df dataset containing slopes
#' @param datetime_col column containing datetime of
#' each concentration measurement
#' @param conc_col column containing gas concentration data
#' @param cut_col column containing cut factor from the
#' flux_fitting function ("cut" or "keep")
#' @param fit_col column containing the modelled fit of the flux
#' @param quality_flag_col column containing the flags produced by flux_quality
#' @param fluxID_col column containing unique IDs for each flux
#' @param fit_slope_col column containing the modelled slope at tz
#' @param b_col column containing the b parameter of the exponential fit
#' @param cor_coef_col column containing the correlation coefficient
#' produced by flux_quality
#' @param RMSE_col column containing the RMSE produced by flux_quality
#' @param start_col column containing the datetime of the start of each flux
#' @param color_discard color for fits with a discard quality flag
#' @param color_cut color for the part of the flux that is cut
#' @param color_keep color for the part of the flux that is kept
#' @param color_ok color for fits with an ok quality flag
#' @param color_zero color for fits with a zero quality flag
#' @param f_date_breaks date_breaks argument for scale_x_datetime
#' @param f_minor_breaks minor breaks argument for scale_x_datetime
#' @param f_date_labels date_labels argument for scale_x_datetime
#' @param f_ylim_upper y axis upper limit
#' @param f_ylim_lower y axis lower limit
#' @param f_scales argument for scales in facet_wrap ("fixed" or "free")
#' @param f_plotname filename for the extracted pdf file
#' @param f_nrow number of row per page in extracted pdf file
#' @param f_ncol ncol argument for facet_wrap
#' @param y_text_position position of the text box
#' @param f_nudge_y to nudge the text box with the parameters above the modelled flux
#' @param print_plot FALSE or TRUE, if TRUE it prints the plot in R
#' but will take time depending on the size of the dataset
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line theme_bw
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off



flux_plot_exp <- function(slopes_df,
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
                          # color_discard = "#D55E00",
                          # color_cut = "#D55E00",
                          # color_keep = "#009E73",
                          # color_ok = "#000000",
                          # color_zero = "#CC79A7",
                          # f_date_breaks = "1 min",
                          # f_minor_breaks = "10 sec",
                          # f_date_labels = "%e/%m \n %H:%M",
                          # f_ylim_upper = 800,
                          # f_ylim_lower = 400,
                          # f_scales = "free",
                          # f_plotname = "plot_quality_exp",
                          # f_ncol = 4,
                          # f_nrow = 3,
                          y_text_position = 500
                          # f_nudge_y = 100,
                          # print_plot = "FALSE"
                          ) {
  # f_scales <- match.arg(f_scales, c("free", "fixed"))
  # f_plotname <- paste("f_quality_plots/", f_plotname, ".pdf", sep = "")

  # folder <- "./f_quality_plots"
  # if (!file.exists(folder)) {
  #   dir.create(folder)
  # }



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
    ggplot(aes(.data$f_datetime)) +
    theme_bw() +
    geom_point(
      aes(y = .data$f_conc, color = .data$f_cut),
      size = 0.2
    ) +
    geom_line(
      aes(y = .data$f_fit, color = .data$f_quality_flag),
      linetype = "longdash"
    ) +
    geom_line(
      aes(y = .data$f_fit_slope, color = .data$f_quality_flag),
      linetype = "dashed"
    ) +
    # scale_color_manual(values = c(
    #   "keep" = ((color_keep)),
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
    geom_text(
      data = param_df,
      aes(x = .data$f_start, y = ((y_text_position)), label = .data$print_col),
      vjust = 0, hjust = "inward"
    ) 
    # #   facet_wrap(~f_fluxID, scales = ((f_scales))) +
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









  # pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
  # for (i in 1:n_pages(plot_exp)) {
  #   print(plot_exp +
  #     facet_wrap_paginate(~f_fluxID,
  #       ncol = ((f_ncol)), nrow = ((f_nrow)),
  #       page = i, scales = ((f_scales))
  #     ))
  # }
  # quietly(dev.off())

  # print("Saving plots in f_quality_plots folder.")

  # if (((print_plot)) == TRUE) {
  #   return(plot_exp)
  # }

  plot_exp
  
}
