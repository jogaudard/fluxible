#' filter cut data before calculating fluxes
#' @param slope_df dataset containing slopes and cut column
#' @param cut_col column containing cutting information
#' @param keep_filter name in cut_col of data to keep

flux_cut <- function(slope_df,
                    cut_col,
                    keep_filter
)
{

    if(is.na(((keep_filter)))) {
      stop("please provide the keep_filter argument to filter the data to keep")
    }

       slope_df <- slope_df |>
      rename(
        f_cut = all_of(((cut_col)))
      ) |>
      filter(.data$f_cut == ((keep_filter)))
}
