#' counts quality flags
#' @description provides a table of how many fluxes were attributed
#' which quality flag.
#' This function is incorporated in flux_quality (output as a message)
#' but can be used alone to extract a dataframe with the flag count.
#' @param slopes_df dataframe of flux slopes
#' @param f_flags list of flags used in the dataset
#' (if different from default from flux_quality).
#' If not provided, it will list only the flags that are
#' present in the dataset (no showing 0).
#' @param fluxid_col column containing fluxes unique ID
#' @param flags_col column containing the quality flags
#' @param cut_col column indicating which part of the flux is being cut
#' @param cut_arg argument defining that the data point should be cut out
#' @return a dataframe with the number of fluxes for each quality flags
#' and their proportion to the total
#' @importFrom dplyr .data rename all_of select group_by summarise
#' tibble right_join filter distinct
#' @importFrom tidyr replace_na
#' @author Vincent Belde
#' @examples
#' data(slopes30qua_flag)
#' flux_flag_count(slopes30qua_flag)
#' @export


flux_flag_count <- function(slopes_df,
                            fluxid_col = f_fluxID,
                            flags_col = f_quality_flag,
                            cut_col = f_cut,
                            f_flags = c(
                              "ok",
                              "discard",
                              "zero",
                              "force_discard",
                              "start_error",
                              "no_data",
                              "force_ok"
                            ),
                            cut_arg = "cut") {
  
  flag_df <- slopes_df |>
    filter({{cut_col}} != cut_arg) |>
    mutate(
      f_quality_flag = as.factor({{flags_col}})
    ) |>
    select({{fluxid_col}}, {{flags_col}}) |>
    distinct()

  flags <- tibble({{flags_col}} := factor(f_flags, levels = f_flags))

  count_table <- flag_df |>
    summarise(
      n = length({{flags_col}}),
      .by = {{flags_col}}
    ) |>
    right_join(flags, by = dplyr::join_by({{flags_col}})) |>
    mutate(
      n = replace_na(.data$n, 0),
      ratio = .data$n / sum(.data$n)
    )

  count_table
}
