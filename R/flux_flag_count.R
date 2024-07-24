#' counts quality flags
#' @description provides a table of how many fluxes were attributed which quality flag
#' @param slopes_df 

flux_flag_count <- function(slopes_df,
                            f_flags = c("ok", "discard", "zero", "weird_flux", "start_error"),
                            fluxID_col = "f_fluxID",
                            flags_col = "f_quality_flag"
                            ){

slopes_df <- slopes_df |>
    rename(
        f_fluxID = all_of(((fluxID_col))),
        f_quality_flag = all_of(((flags_col)))
    )

                                flag_df <- slopes_df |>
                                                mutate(
                                                    f_quality_flag = as.factor(f_quality_flag)
                                                ) |>
                                                select(f_fluxID, f_quality_flag) |>
                                                unique()

                                flags <- tibble(f_quality_flag = ((f_flags)))

                                count_table <- flag_df  |>
                                                    group_by(f_quality_flag) |>
                                                    summarise(
                                                        n = length(f_quality_flag)
                                                    ) |>
                                                    right_join(flags) |>
                                                    mutate(
                                                        n = replace_na(n, 0),
                                                        ratio = n/sum(n)
                                                    )

                                count_table
                            }

flux_flag_count(slopes30_flag)
