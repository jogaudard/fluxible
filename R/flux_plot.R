#' ploting fluxes for fit evaluation
#' @description plots the fluxes and indicates what should be discarded or replaced by zero
#' @param fit_type model used in flux_fitting, exponential or linear
#' @param slopes_df dataset containing slopes
#' @param datetime_col column containing datetime of each concentration measurement
#' @param conc_col column containing gas concentration data
#' @param cut_col column containing cut factor from the flux_fitting function ("cut" or "keep")
#' @param fit_col column containing the modelled fit of the flux
#' @param quality_flag_col column containing the flags produced by flux_quality
#' @param fluxID_col column containing unique IDs for each flux
#' @param pvalue_col column containing the p-value of each flux
#' @param rsquared_col column containing the r squared to be used for the quality assessment
#' @param fit_slope_col column containing the modelled slope at tz
#' @param b_col column containing the b parameter of the exponential fit
#' @param cor_coef_col column containing the correlation coefficient produced by flux_quality
#' @param RMSE_col column containing the RMSE produced by flux_quality
#' @param start_col column containing the datetime of the start of each flux
#' @param f_date_breaks date_breaks argument for scale_x_datetime
#' @param f_minor_breaks minor breaks argument for scale_x_datetime
#' @param f_date_labels date_labels argument for scale_x_datetime
#' @param f_ylim_upper y axis upper limit
#' @param f_ylim_lower y axis lower limit
#' @param f_scales argument for scales in facet_wrap ("fixed" or "free")
#' @param f_plotname filename for the extracted pdf file
# #' @param f_paper = "a4r", for next version of package, paper size
#' @param f_nrow number of row per page in extracted pdf file
#' @param f_ncol ncol argument for facet_wrap
#' @param print_plot FALSE or TRUE, if TRUE it prints the plot in R but will take time depending on the size of the dataset
#' @importFrom dplyr rename select distinct mutate
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual scale_x_datetime ylim facet_wrap labs geom_text
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom purrr quietly
#' @examples
#' data(slopes0_flag)
#' flux_plot(slopes0_flag, fit_type = "exp", datetime_col = "datetime", fit_slope_col = "fit_slope",
#' start_col = "start", print_plot = TRUE)
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
                            f_date_breaks = "1 min",
                            f_minor_breaks = "10 sec",
                            f_date_labels = "%e/%m \n %H:%M",
                            f_ylim_upper = 800,
                            f_ylim_lower = 400,
                            f_scales = "free",
                            f_plotname = "plot_quality",
                            # f_paper = "a4r",
                            f_ncol = 4,
                            f_nrow = 3,
                            print_plot = "FALSE"

){
    fit_type <- match.arg(((fit_type)), c("exponential", "linear"))

 if(((fit_type)) == "exponential") {
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
            RMSE_col = ((RMSE_col)),
            f_date_breaks = ((f_date_breaks)),
            f_minor_breaks = ((f_minor_breaks)),
            f_date_labels = ((f_date_labels)),
            f_ylim_upper = ((f_ylim_upper)),
            f_ylim_lower = ((f_ylim_lower)),
            f_scales = ((f_scales)),
            f_plotname = ((f_plotname)),
            f_ncol = ((f_ncol)),
            f_nrow = ((f_nrow)),
            print_plot = ((print_plot))
        )
    }

   
    if(((fit_type)) == "linear") {
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
            start_col = ((start_col)),
            f_date_breaks = ((f_date_breaks)),
            f_minor_breaks = ((f_minor_breaks)),
            f_date_labels = ((f_date_labels)),
            f_ylim_upper = ((f_ylim_upper)),
            f_ylim_lower = ((f_ylim_lower)),
            f_scales = ((f_scales)),
            f_plotname = ((f_plotname)),
            f_ncol = ((f_ncol)),
            f_nrow = ((f_nrow)),
            print_plot = ((print_plot))
        )
    }

f_plot
    

}