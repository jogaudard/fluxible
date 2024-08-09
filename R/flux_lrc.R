

flux_lrc <- function(fluxes_df,
                    flux_col = "fluxes",
                    type_col = "type",
                    lrc_arg = "LRC",
                    nee_arg = "NEE",
                    er_arg = "ER",
                    lrc_group = c(),
                    PARfix = 300,
                    PARnull = 0
                    ){

  coefficients_lrc <- fluxes_df %>%
    filter(
        type == ((lrc_arg))
    ) |>
    group_by_at(vars(all_of(((lrc_group))))) |>
    nest %>% 
    mutate(lm = map(data, ~ lm(flux ~ PARavg + I(PARavg^2), data = .x)),
           table = map(lm, tidy),
           table = map(table, select, term, estimate),
           table = map(table, pivot_wider, names_from = term, values_from = estimate)
           
    ) %>% 
    unnest(table) %>% 
    select(all_of(group), `(Intercept)`, PARavg, `I(PARavg^2)`) %>% 
    rename(
      origin = "(Intercept)",
      a = "I(PARavg^2)",
      b = "PARavg"
    )
  
  flux_corrected_PAR <- fluxes_df %>% 
    filter(
      type %in% c("NEE", "ER")
    ) %>% 
    left_join(coefficients_lrc) %>% 
    mutate(
      PAR_corrected_flux = 
        case_when( #we correct only the NEE
          type == "NEE" ~ flux + a * (PARfix^2 - PARavg^2) + b * (PARfix - PARavg),
          type == "ER" ~ flux + a * (PARnull^2 - PARavg^2) + b * (PARnull - PARavg)
        )
    ) %>% 
    select(!c(a, b, origin))
}