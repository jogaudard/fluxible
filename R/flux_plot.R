#' ploting fluxes for fit evaluation
#' @description plots the fluxes and indicates what should be discarded
#' or replaced by zero
#' @param fit_type model used in flux_fitting, exponential or linear
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
#' @param rsquared_col column containing the r squared to be used
#' for the quality assessment
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
#' @param output if "pdfpages", the plots are saved as A4 landscape pdf pages (default);
#' if "ggsave", the plots can be saved with the ggsave function
#' @param device see ggsave()
#' @param path see ggsave()
#' @param scale see ggsave()
#' @param width see ggsave()
#' @param height see ggsave()
#' @param units see ggsave()
#' @param dpi see ggsave()
#' @param limitsize see ggsave()
#' @param bg see ggsave()
#' @param create.dir see ggsave()
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual
#' scale_x_datetime ylim facet_wrap labs geom_text theme_bw ggsave
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @examples
#' data(slopes0_flag)
#' flux_plot(slopes0_flag, fit_type = "exp", fit_slope_col = "f_fit_slope", print_plot = TRUE)
#' data(slopes30lin_flag)
#' flux_plot(slopes30lin_flag, fit_type = "lin", print_plot = TRUE, f_plotname = "pdf_quality_plots")
#' flux_plot(slopes30lin_flag, fit_type = "lin", print_plot = TRUE, output = "ggsave", device = "jpg", f_plotname = "jpg_quality_plots")
#' flux_plot(slopes30lin_flag, fit_type = "lin", print_plot = TRUE, output = "ggsave", f_plotname = "jpg_quality_plots.jpg")
#' @export

flux_plot <- function(slopes_df,
                      fit_type,
                      datetime_col = "f_datetime",
                      conc_col = "f_conc",
                      cut_col = "f_cut",
                      fit_col = "f_fit",
                      fit_slope_col = "f_fit_slope",
                      quality_flag_col = "f_quality_flag",
                      fluxID_col = "f_fluxID",
                      pvalue_col = "f_pvalue",
                      rsquared_col = "f_rsquared",
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
                      f_plotname = "plot_quality",
                      f_ncol = 4,
                      f_nrow = 3,
                      y_text_position = 500,
                      f_nudge_y = 100,
                      print_plot = "FALSE",
                      output = "pdfpages",
                      device = NULL,
                      path = NULL,
                      scale = 1,
                      width = NA,
                      height = NA,
                      units = c("in", "cm", "mm", "px"),
                      dpi = 300,
                      limitsize = TRUE,
                      bg = NULL,
                      create.dir = FALSE
                      ) {
  fit_type <- match.arg(((fit_type)), c("exponential", "linear"))

  f_scales <- match.arg(f_scales, c("free", "fixed"))
  f_plotname <- paste("f_quality_plots/", f_plotname, sep = "")

  folder <- "./f_quality_plots"
  if (!file.exists(folder)) {
    dir.create(folder)
  }

  if (((fit_type)) == "exponential") {
    f_plot <- flux_plot_exp(
      ((slopes_df)),
      datetime_col = ((datetime_col)),
      conc_col = ((conc_col)),
      cut_col = ((cut_col)),
      fit_col = ((fit_col)),
      fit_slope_col = ((fit_slope_col)),
      quality_flag_col = ((quality_flag_col)),
      fluxID_col = ((fluxID_col)),
      start_col = ((start_col)),
      b_col = ((b_col)),
      cor_coef_col = ((cor_coef_col)),
      RMSE_col = ((RMSE_col))
    )
  }


  if (((fit_type)) == "linear") {
    f_plot <- flux_plot_lin(
      ((slopes_df)),
      datetime_col = ((datetime_col)),
      conc_col = ((conc_col)),
      cut_col = ((cut_col)),
      fit_col = ((fit_col)),
      quality_flag_col = ((quality_flag_col)),
      pvalue_col = ((pvalue_col)),
      rsquared_col = ((rsquared_col)),
      fluxID_col = ((fluxID_col)),
      start_col = ((start_col))
    )
  }

  f_plot <- f_plot +
    scale_color_manual(values = c(
      "keep" = ((color_keep)),
      "cut" = ((color_cut)),
      "ok" = ((color_ok)),
      "discard" = ((color_discard)),
      "zero" = ((color_zero)),
      "start_error" = ((color_discard)),
      "weird_flux" = ((color_discard))
    )) +
    scale_x_datetime(
      date_breaks = ((f_date_breaks)), minor_breaks = ((f_minor_breaks)),
      date_labels = ((f_date_labels))
    ) +
    ylim(((f_ylim_lower)), ((f_ylim_upper))) +
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

if(((output)) == "pdfpages") {
  f_plotname <- paste(f_plotname, ".pdf", sep = "")
    pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
  for (i in 1:n_pages(f_plot)) {
    print(f_plot +
      facet_wrap_paginate(
        ~f_fluxID,
        ncol = ((f_ncol)), nrow = ((f_nrow)),
        page = i, scales = ((f_scales))
      ))
  }
  quietly(dev.off())
}

if(((output)) == "ggsave"){
  ggsave(
    ((f_plotname)),
    plot = f_plot,
    device = ((device))
  )
}

  print("Saving plots in f_quality_plots folder.")


  if (((print_plot)) == TRUE) {
    return(f_plot)
  }

  
}
