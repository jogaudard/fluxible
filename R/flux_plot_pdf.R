#' print plots in pdf
#' @param f_plot plot from flux_plot
#' @param facet_wrap_args list of arguments for
#' \link[ggforce:facet_wrap_paginate]{facet_wrap_paginate}
#' @param f_plotname name of plot
#' @param plot_pages number of pages
#' @importFrom ggforce facet_wrap_paginate
#' @importFrom progress progress_bar


flux_plot_pdf <- function(f_plot,
    f_plotname,
    plot_pages,
    facet_wrap_args
) {
    f_plotname <- paste(f_plotname, ".pdf", sep = "")
    pdf(f_plotname, paper = "a4r", width = 11.7,
        height = 8.3, title = f_plotname)
    pb <- progress_bar$new(
      format =
        "Printing plots in pdf document [:bar] :current/:total (:percent)",
      total = plot_pages
    )
    pb$tick(0)
    Sys.sleep(0.5)
    for (i in 1:plot_pages) {
      pb$tick()
      Sys.sleep(0.001)
      print(f_plot +
        do.call(facet_wrap_paginate,
          args = c(
            facets = ~f_facetid,
            page = i,
            facet_wrap_args
          )
        ))
    }
    quietly(dev.off())
    message("Plots saved in f_quality_plots folder.")
}