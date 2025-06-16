#' Returns a unit conversion coefficient for flux_calc
#' @param flux_units desired units for the calculated fluxes
#' 
#' @details 


flux_units <- function(flux_units,
                       amount_units = c("mol", "mmol", "umol", "nmol", "pmol"),
                       time_units = c("d", "h", "s"),
                       surface_units = c("m2", "dm2", "cm2")) {

  amount <- str_extract(flux_units, "^\\w*")
  time <- str_extract(flux_units, "(?<=\\/)(\\w.*)(?=\\/)")
  surface <- str_extract(flux_units, "\\w*$")

  amount <- match.arg(amount, amount_units)
  time <- match.arg(time, time_units)
  surface <- match.arg(surface, surface_units)

  # output units in flux_calc are micromol/s/m^2

  amount_coeff <- case_when(
    amount == "mol" ~ 1e-6,
    amount == "mmol" ~ 1e-3,
    amount == "umol" ~ 1,
    amount == "nmol" ~ 1e3,
    amount == "pmol" ~ 1e6
  )

  time_coeff <- case_when(
    time == "d" ~ 86400,
    time == "h" ~ 3600,
    time == "s" ~ 1
  )

  surface_coeff <- case_when(
    surface == "m2" ~ 1,
    surface == "dm2" ~ 1e-2,
    surface == "cm2" ~ 1e-4
  )

  flux_coeff <- amount_coeff * time_coeff * surface_coeff

  flux_coeff
}
