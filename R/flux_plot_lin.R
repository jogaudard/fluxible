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
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared
#' used for the quality assessment
#' @param start_col column containing the datetime of the start of each flux
#' @param f_date_breaks date_breaks argument for scale_x_datetime
#' @param f_minor_breaks minor breaks argument for scale_x_datetime
#' @param f_date_labels date_labels argument for scale_x_datetime
#' @param f_ylim_upper y axis upper limit
#' @param f_ylim_lower y axis lower limit
#' @param f_scales argument for scales in facet_wrap ("fixed" or "free")
#' @param f_plotname filename for the extracted pdf file
#' @param f_nrow number of row per page in extracted pdf file
#' @param f_ncol ncol argument for facet_wrap
#' @param print_plot FALSE or TRUE, if TRUE it prints the plot in R
#' but will take time depending on the size of the dataset
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line
#' scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @importFrom grDevices pdf dev.off
#' @examples
#' data(slopes0lin_flag)
#' flux_plot_lin(slopes0lin_flag,
#'   datetime_col = "datetime", cut_col = "cut", fit_col = "fit",
#'   start_col = "start", print_plot = TRUE
#' )
#' data(slopes30lin_flag)
#' flux_plot_lin(slopes30lin_flag,
#'   datetime_col = "datetime", cut_col = "cut", fit_col = "fit",
#'   start_col = "start", print_plot = TRUE
#' )
#' @export
#'
#'

flux_plot_lin <- function(slopes_df,
                          datetime_col = "f_datetime",
                          conc_col = "f_conc",
                          cut_col = "f_cut",
                          fit_col = "f_fit",
                          quality_flag_col = "f_quality_flag",
                          fluxID_col = "f_fluxID",
                          pvalue_col = "f_pvalue",
                          rsquared_col = "f_rsquared",
                          start_col = "f_start",
                          f_date_breaks = "1 min",
                          f_minor_breaks = "10 sec",
                          f_date_labels = "%e/%m \n %H:%M",
                          f_ylim_upper = 800,
                          f_ylim_lower = 400,
                          f_scales = "free",
                          f_plotname = "plot_quality_lin",
                          f_ncol = 4,
                          f_nrow = 3,
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
      f_quality_flag = all_of((quality_flag_col)),
      f_fluxID = all_of((fluxID_col)),
      f_pvalue = all_of((pvalue_col)),
      f_rsquared = all_of((rsquared_col)),
      f_start = all_of(((start_col)))
    )

  param_df <- slopes_df |>
    select("f_conc", "f_start", "f_fluxID", "f_rsquared", "f_pvalue") |>
    group_by(.data$f_fluxID) |>
    mutate(
      conc_start = .data$f_conc[1]
    ) |>
    ungroup() |>
    select(!"f_conc") |>
    distinct() |>
    mutate(
      f_rsquared = round(.data$f_rsquared, digits = 2),
      f_pvalue = round(.data$f_pvalue, digits = 4),
      print_col = paste(
        "R2 = ", .data$f_rsquared, "\n", "p-value = ", .data$f_pvalue,
        sep = ""
      )
    )

  plot_lin <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    geom_point(aes(y = .data$f_conc, color = .data$f_cut), size = 0.2) +
    geom_line(
      aes(y = .data$f_fit, color = .data$f_quality_flag),
      linetype = "longdash"
    ) +
    scale_color_manual(values = c(
      "keep" = "green",
      "cut" = "red",
      "ok" = "black",
      "discard" = "red",
      "zero" = "grey",
      "start_error" = "red",
      "weird_flux" = "purple"
    )) +
    scale_x_datetime(
      date_breaks = ((f_date_breaks)), minor_breaks = ((f_minor_breaks)),
      date_labels = ((f_date_labels))
    ) +
    ylim(((f_ylim_lower)), ((f_ylim_upper))) +
    geom_text(
      data = param_df,
      aes(
        x = .data$f_start, y = .data$conc_start,
        label = .data$print_col
      ),
      vjust = 0, hjust = "inward", nudge_y = 100
    ) +
    #   facet_wrap(~f_fluxID, scales = ((f_scales))) +
    facet_wrap_paginate(
      ~f_fluxID,
      ncol = ((f_ncol)), nrow = ((f_nrow)), scales = ((f_scales))
    ) +
    labs(
      title = "Fluxes quality assessment",
      x = "Datetime",
      y = "Concentration",
      colour = "Quality flags"
    )









  pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
  for (i in 1:n_pages(plot_lin)) {
    print(plot_lin +
      facet_wrap_paginate(
        ~f_fluxID,
        ncol = ((f_ncol)), nrow = ((f_nrow)),
        page = i, scales = ((f_scales))
      ))
  }
  quietly(dev.off())

  print("Saving plots in f_quality_plots folder.")


  if (((print_plot)) == TRUE) {
    return(plot_lin)
  }
}