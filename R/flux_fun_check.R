#' checking that arguments and columns are in the correct class
#' @param args list of arguments or dataframe to check
#' @param fn list of functions used to check (`is.numeric`, `is.character`, ...)
#' @param msg list of messages to return in case of failed check
#' @param origdf in case args is a df with selected columns to check origdf
#' is the orginal df to take the name from for a more obvious error message
#' @author Adam Klimes



flux_fun_check <- function(args,
                           fn,
                           msg,
                           origdf = NA) {
  df_name <- if (is.data.frame(args)) deparse(substitute(origdf)) else NA
  mapply(flux_check_item, args, fn, msg, names(args), df_name = df_name)
}
