# just a script to produce plots to include in the poster
# we can use the lia data used in the readme
library(fluxible)
library(progress)
library(tidyverse)

conc_liahovden <- flux_match(
  co2_liahovden,
  record_liahovden
)
slopes_exp_liahovden <- flux_fitting(
  conc_liahovden,
  fit_type = "exponential"
)
slopes_exp_liahovden <- flux_quality(
  slopes_exp_liahovden,
  fit_type = "expo",
  slope_col = "f_slope"
  )

# special function to make the plots for the poster (bigger and co)
flux_plot_exp_poster <- function(slopes_df,
                      color_discard = "#D55E00",
                      color_cut = "#D55E00",
                      color_ok = "#009E73",
                      color_zero = "#CC79A7",
                      linewidth = 1,
                      size_point = 1,
                      f_date_breaks = "1 min",
                      f_minor_breaks = "10 sec",
                      f_date_labels = "%e/%m \n %H:%M",
                      f_ylim_upper = 800,
                      f_ylim_lower = 400,
                      f_plotname = "plot_quality",
                      facet_wrap_args = list(
                        ncol = 2,
                        nrow = 2,
                        scales = "free"
                      ),
                      y_text_position = 500,
                      cut_arg = "cut",
                      no_data_flag = "no_data") {
  

  slopes_df <- slopes_df |>
    filter(
      .data$f_quality_flag != ((no_data_flag))
    )




  param_df <- flux_param_exp(
    ((slopes_df)),
    cut_arg = ((cut_arg))
  )

  slopes_df <- flux_plot_flag(((slopes_df)),
    ((param_df)),
    cut_arg = ((cut_arg))
  )

  slopes_df <- slopes_df |>
    rename(
      fit = "f_fit",
      slope = "f_fit_slope"
    ) |>
    pivot_longer(
      cols = c("fit", "slope"),
      names_to = "linetype",
      values_to = "fit"
    )



  f_plot <- slopes_df |>
    ggplot(aes(.data$f_datetime)) +
    geom_point(
      aes(y = .data$f_conc, color = .data$f_quality_flag),
      size = ((size_point)),
      na.rm = TRUE
    ) +
    geom_text(
      aes(x = .data$f_start, y = ((y_text_position)), label = .data$print_col),
      vjust = 0, hjust = "inward",
      na.rm = TRUE
    )

  message("Plotting in progress")

  f_plot <- f_plot +
    geom_line(
      aes(y = .data$fit, linetype = .data$linetype),
      linewidth = ((linewidth)),
      na.rm = TRUE,
      show.legend = TRUE
    ) +
    scale_color_manual(values = c(
      "cut" = ((color_cut)),
      "ok" = ((color_ok)),
      "discard" = ((color_discard)),
      "zero" = ((color_zero)),
      "start_error" = ((color_discard)),
      "weird_flux" = ((color_discard)),
      "force_ok" = ((color_ok))
    )) +
    scale_linetype_manual(values = c(
      "fit" = "longdash",
      "slope" = "dashed"
    )) +
    scale_x_datetime(
      date_breaks = ((f_date_breaks)), minor_breaks = ((f_minor_breaks)),
      date_labels = ((f_date_labels))
    ) +
    ylim(((f_ylim_lower)), ((f_ylim_upper))) +
    do.call(facet_wrap_paginate,
      args = c(facets = ~f_fluxID, ((facet_wrap_args)))
    ) +
    labs(
      title = "Fluxes quality assessment",
      subtitle = "Exponential model",
      x = "Datetime",
      y = "Concentration",
      colour = "Quality flags",
      linetype = "Fits"
    ) +
    guides(color = guide_legend(override.aes = list(linetype = 0))) +
    theme_classic()


    # ggsave("poster_plot.png", width = 255, height = 170, units = c("mm"), dpi = 300,)

    f_plotname <- paste(f_plotname, ".pdf", sep = "")
    pdf(((f_plotname)), paper = "a4r", width = 11.7, height = 8.3)
    pb <- progress_bar$new(
      format =
        "Printing plots in pdf document [:bar] :current/:total (:percent)",
      total = n_pages(f_plot)
    )
    pb$tick(0)
    Sys.sleep(3)
    for (i in 1:n_pages(f_plot)) {
      pb$tick()
      Sys.sleep(0.1)
      print(f_plot +
        do.call(facet_wrap_paginate,
          args = c(
            facets = ~f_fluxID,
            page = i,
            ((facet_wrap_args))
          )
        ))
    }
    quietly(dev.off())
    
}



slopes_exp_liahovden |>
  dplyr::filter(f_fluxID %in% c(28, 51, 100)) |> # we just show a sample of the plots to avoid slowing down the example
    mutate(
        f_fluxID = case_when(
            f_fluxID == 28 ~ "A",
            f_fluxID == 51 ~ "B",
            f_fluxID == 100 ~ "C"
        ),
        f_fluxID = factor(f_fluxID, levels = c("A", "B", "C"))
    ) |>
    # view()
    flux_plot_exp_poster(
      linewidth = 0.8,
      size_point = 1,
      f_plotname = "poster_plot",
      f_ylim_lower = 375,
      f_ylim_upper = 525,
      y_text_position = 470
      )
