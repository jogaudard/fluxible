#' Calculates GEP
#' @description to calculate gross ecosystem production (GEP) from net ecosystem
#' (NEE) exchange and ecosystem respiration (ER) as GEP = NEE - ER.
#' Datetime, PAR and other variables to keep will be taken from the NEE
#' measurement. If it is missing, GEP will be dropped for that pair.
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
#' cur_group_id
#' @importFrom tidyr pivot_wider fill
#' @importFrom purrrlyr slice_rows unslice
#' @examples
#' data(co2_fluxes)
#' flux_gep(co2_fluxes, id_cols = "turfID", flux_col = "flux",
#' type_col = "type", datetime_col = "f_start", par_col = "PAR",
#' cols_keep = c("temp_soil"))
#' @export

flux_gep <- function(fluxes_df,
                     type_col,
                     datetime_col,
                     par_col,
                     flux_col = f_flux,
                     id_cols,
                     nee_arg = "NEE",
                     er_arg = "ER",
                     cols_keep = c()) {

  name <- deparse(substitute(fluxes_df))

  fluxes_df_check <- fluxes_df |>
    select({{flux_col}})

  fluxes_df_ok <- flux_fun_check(fluxes_df_check,
                                 fn = list(is.numeric),
                                 msg = "has to be numeric",
                                 name_df = name)


  if (!fluxes_df_ok)
    stop("Please correct the arguments", call. = FALSE)


  fluxes_df <- fluxes_df |>
    mutate(
      id = dplyr::cur_group_id(),
      .by = all_of(id_cols)
    )

  select_df <- fluxes_df |>
    select(
      "id",
      all_of(c(cols_keep, id_cols)),
      {{type_col}},
      {{par_col}},
      {{type_col}},
      {{flux_col}},
      {{datetime_col}}
    )

  fluxes_gep <- fluxes_df |>
    select(
      {{flux_col}},
      {{type_col}},
      {{datetime_col}},
      {{par_col}},
      "id"
    ) |>
    mutate(
      type = case_when(
        .data$type == nee_arg ~ "NEE",
        .data$type == er_arg ~ "ER"
      )
    ) |>
    filter(
      .data$type == "NEE" |
        .data$type == "ER"
    )

  fluxes_gep <- fluxes_gep |>
  rename(
    f_flux = {{flux_col}},
    f_datetime = {{datetime_col}},
    f_par = {{par_col}}
  ) |>
    pivot_wider(id_cols = "id",
      names_from = {{type_col}},
      values_from = c("f_flux", "f_datetime", "f_par")
    ) |>
    rename(
      {{par_col}} := "f_par_NEE",
      {{datetime_col}} := "f_datetime_NEE"
    ) |>
    mutate(
      {{flux_col}} := .data$f_flux_NEE - .data$f_flux_ER,
      {{type_col}} := "GEP"
    ) |>
    select(
      {{datetime_col}},
      "id",
      {{par_col}},
      {{type_col}},
      {{flux_col}}
    )

  id_cols_df <- fluxes_df |>
    select(all_of(id_cols), "id")

  nee_missing <- fluxes_gep |>
    filter(
      is.na({{datetime_col}})
    ) |>
    select("id") |>
    left_join(id_cols_df, by = "id")

  nee_missing[] <- Map(paste, names(nee_missing), nee_missing, sep = ": ")

  nee_missing <- nee_missing |>
    mutate(
      msg = apply(nee_missing[, id_cols], 1, paste, collapse = ", "),
      f_warnings = paste(
        "\n", "NEE missing for measurement", .data$msg
      )
    ) |>
    pull(.data$f_warnings)

  fluxes_gep <- fluxes_gep |>
    drop_na({{datetime_col}})

  join_arg <- dplyr::join_by(
    "id" == "id",
    {{par_col}} == {{par_col}},
    {{type_col}} == {{type_col}},
    {{flux_col}} == {{flux_col}},
    {{datetime_col}} == {{datetime_col}}
  )

  fluxes_gep <- fluxes_gep |>
    full_join(
      select_df,
      by = join_arg
    ) |>
    group_by(.data$id) |>
    fill(all_of(c(cols_keep, id_cols)), .direction = "updown") |>
    ungroup() |>
    select(!"id")

  f_warnings <- stringr::str_c(nee_missing)


  if (any(!is.na(nee_missing))) warning(f_warnings)

  fluxes_gep

}
