#' @description separate fluxes in segments of similar slopes with stable PAR
#' @outputs a df with modelled gas concentration (NA outside of selected segments)
#' flag cut/keep indicatin if the row is used in a segment or not (needed for plotting)
#' ideally flux_segment would be integrated in flux_fitting as an option