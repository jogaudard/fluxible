#' Graphing fluxes for fit evaluation
#' @description graphs the fluxes and indicates what should be discarded or replaced by zero
#' @param slopes_df dataset containing slopes
#' @param 
#' @importFrom dplyr rename
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_manual scale_x_datetime ylim facet_wrap labs
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @examples
#' data(slopes0lin_flag)
#' flux_graph_lin(slopes0lin_flag, datetime_col = "datetime", conc_col = "conc", cut_col = "cut", fit_col = "fit", print_plot = TRUE)
#' @export
#' 
#' 

# to do
# american format
# print r.squared and co on plots


flux_graph_lin <- function(slopes_df,
                            datetime_col = "f_datetime",
                            conc_col = "f_conc",
                            cut_col = "f_conc",
                            fit_col = "f_fit",
                            quality_flag_col = "f_quality_flag",
                            fluxID_col = "f_fluxID",
                            f_date_breaks = "1 min",
                            f_minor_breaks = "10 sec",
                            f_date_labels = "%e/%m \n %H:%M",
                            f_ylim_upper = 800,
                            f_ylim_lower = 400,
                            f_scales = "free",
                            f_plotname = "plot_quality_lin.pdf",
                            # f_paper = "a4r",
                            f_ncol = 5,
                            f_nrow = 4,
                            print_plot = "FALSE"
){  
    slopes_df <- slopes_df |>
        rename(
            f_datetime = all_of((datetime_col)),
            f_conc = all_of((conc_col)),
            f_cut = all_of((cut_col)),
            f_fit = all_of((fit_col)),
            f_quality_flag = all_of((quality_flag_col)),
            f_fluxID = all_of((fluxID_col))
        )

    graph_lin <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    geom_point(aes(y = .data$f_conc, color = .data$f_cut), size = 0.2) +
    geom_line(aes(y = .data$f_fit, color = .data$f_quality_flag), linetype = "longdash") +
    scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red",
    "ok" = "black",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red",
    "weird_flux" = "purple"
  )) +
  scale_x_datetime(date_breaks = ((f_date_breaks)), minor_breaks = ((f_minor_breaks)), date_labels = ((f_date_labels))) +
  ylim(((f_ylim_lower)), ((f_ylim_upper))) +
#   facet_wrap(~f_fluxID, scales = ((f_scales))) +
  facet_wrap_paginate(~f_fluxID, ncol = ((f_ncol)), nrow = ((f_nrow)), scales = ((f_scales))) +
  labs(
    title = "Fluxes quality assessment",
    x = "Datetime",
    y = "Concentration",
    colour = "Quality flags"
  )

if(((print_plot)) == TRUE) {print(graph_lin)}
  


  

  #removing if there is any file 
file.remove((f_plotname))

pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
for(i in 1:n_pages(graph_lin)){
print(graph_lin +
facet_wrap_paginate(~ f_fluxID, ncol = ((f_ncol)), nrow = ((f_nrow)), page = i, scales = ((f_scales))))
}
dev.off()

}
