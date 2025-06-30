#' print plots in pdf
#' @param f_plot plot from flux_plot
#' @param nb_fluxid number of facets
#' @param f_plotname name of plot
#' @param plotly_args arguments for plotly in the form
#' `list(ncol, width, ratio)`
#' @importFrom htmlwidgets saveWidget
#' @importFrom ggplot2 facet_wrap theme coord_fixed
#' @importFrom plotly ggplotly style


flux_plot_plotly <- function(f_plot,
  f_plotname,
  nb_fluxid,
  plotly_args
) {
  f_plotname <- paste(f_plotname, ".html", sep = "")


  f_ncol <- plotly_args$ncol
  width <- plotly_args$width
  ratio <- plotly_args$ratio

  f_plot <- f_plot +
    facet_wrap(~f_facetid, ncol = f_ncol, scales = "free")
  
  f_height <- (((width) / f_ncol)
               * ceiling((nb_fluxid / f_ncol)) / ratio)

  f_plotly <- ggplotly(
    f_plot,
    width = width, height = f_height
  ) |>
  style(textposition = c("right", "top"))

  saveWidget(f_plotly, file = f_plotname)

  message("Plots saved in f_quality_plots folder.")
}