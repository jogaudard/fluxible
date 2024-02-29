#' to provide quality assessment for the fluxes calculated with the linear model


flux_quality_lin <- function(fluxes_df,
                            pvalue_threshold = 0.3,
                            rsquared_threshold = 0.7,
                            blind_replace = FALSE, # if TRUE, it replaces the data by NA or 0 according to the quality flags
                            fluxID_col = "fluxID",
                            fluxes_col = "fluxes",
                            pvalue_col = "p.value",
                            rsquared_col = "r.squared"
){

    fluxes_df <- fluxes_df |>
        rename(
            fluxID = all_of((fluxID_col)),
            fluxes = all_of((fluxes_col)),
            pvalue = all_of((pvalue_col)),
            rsquared = all_of((rsquared_col))
        )

    fluxes_df <- fluxes_df |>
        mutate(
            quality_flag = case_when(
                .data$rsquared >= ((rsquared_threshold)) ~ "ok",
                .data$rsquared < ((rsquared_threshold)) & .data$pvalue >= ((pvalue_threshold)) ~ "discard",
                .data$rsquared < ((rsquared_threshold)) & .data$pvalue < ((pvalue_threshold)) ~ "zero"
            ),
            fluxes = case_when(
                ((blind_replace)) == FALSE ~ .data$fluxes,
                ((blind_replace)) == TRUE ~ case_when(
                    .data$quality_flag == "ok" ~ .data$fluxes,
                    .data$quality_flag == "discard" ~ NA_real_,
                    .data$quality_flag == "zero" ~ 0
                )
            )
        )
}