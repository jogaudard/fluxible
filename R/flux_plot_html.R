#' print plots in pdf
#' @param f_plot plot from flux_plot
#' @param facet_wrap_args list of arguments for
#' \link[ggforce:facet_wrap_paginate]{facet_wrap_paginate}
#' @param f_plotname name of plot
#' @importFrom htmlwidgets saveWidget
#' @importFrom ggplot2 facet_wrap
#' @importFrom plotly ggplotly


flux_plot_pdf <- function(f_plot,
  f_plotname,
  facet_wrap_args
) {
  f_plotname <- paste(f_plotname, ".html", sep = "")

  f_plot <- f_plot +
    do.call(facet_wrap, # do.call is here to pass arguments as a list
      args = c(facets = ~f_facetid, facet_wrap_args)
    )

  saveWidget(ggplotly(f_plot), file = f_plotname)

  message("Plots saved in f_quality_plots folder.")
}