#' @importFrom purrrlyr slice_rows unslice

flux_gep <- function(fluxes_df,
                     id_cols, #must be factor
                     flux_col,
                     type_col, # must be factor
                     datetime_col,
                     par_col,
                     nee_arg = "NEE",
                     er_arg = "ER",
                     cols_keep
){
  # dummy check

fluxes_df <- fluxes_df |>
  rename(
    flux = all_of(((flux_col))),
    type = all_of(((type_col))),
    datetime = all_of(((datetime_col))),
    PAR = all_of(((par_col)))
  )

fluxes_gep <- fluxes_df |>
  select(
    "flux",
    "type",
    "datetime",
    "PAR",
    all_of(((id_cols)))
  ) |>
  mutate(
    type = case_when(
      .data$type == ((nee_arg)) ~ "NEE",
      .data$type == ((er_arg)) ~ "ER"
    )
  ) |>
  filter(
    .data$type == "NEE" |
      .data$type == "ER"
  )

  fluxes_gep <- fluxes_gep |>
    pivot_wider(all_of(((id_cols))),
                names_from = .data$type,
                values_from = c(.data$flux, .data$datetime, .data$PAR)
    ) |>
    rename(
      ER = flux_ER,
      NEE = flux_NEE,
      PAR = PAR_NEE,
      datetime = datetime_NEE
    ) |>
    mutate(
      flux = NEE - ER,
      type = "GEP"
    ) |>
    select(.data$datetime, all_of(((id_cols))), .data$PAR, .data$type, .data$flux)
  

  
  fluxes_gep <- fluxes_gep |>
    full_join(fluxes_df, by = c(all_of(((id_cols))), "PAR", "type", "flux", "datetime")) |>
    slice_rows(all_of(((id_cols)))) |>
    fill(all_of(((cols_keep))), .direction = "up") |>
    unslice()
  
fluxes_gep
}