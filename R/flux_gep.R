#' Calculates GEP
#' @description to calculate gross ecosystem production (GEP) from net ecosystem
#' (NEE) exchange and ecosystem respiration (ER) as GEP = NEE - ER
#' @param fluxes_df a dataframe containing NEE and ER
#' @param id_cols columns used to identify each pair of ER and NEE
#' @param flux_col column containing flux values
#' @param type_col column containing type of flux (NEE or ER)
#' @param datetime_col column containing start of measurement as datetime
#' @param par_col column containing PAR values for each flux
#' @param nee_arg argument designating NEE fluxes in type column
#' @param er_arg argument designating ER fluxes in type column
#' @param cols_keep columns to keep from fluxes_df. Values from NEE row will be
#' filled in GEP row.
#' @return a df with GEP as a flux type, with PAR and datetime from the NEE
#' measurement for each pair of ER and NEE
#' @importFrom dplyr rename select mutate case_when filter full_join
#' @importFrom tidyr pivot_wider fill
#' @importFrom purrrlyr slice_rows unslice
#' @examples
#' 
#' @export

flux_gep <- function(fluxes_df,
                     id_cols, #must be factor
                     flux_col,
                     type_col, # must be factor
                     datetime_col,
                     par_col,
                     nee_arg = "NEE",
                     er_arg = "ER",
                     cols_keep = c()
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