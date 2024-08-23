#' checking that arguments and columns are in the correct class
#' @param args list of arguments or dataframe to check
#' @param fn list of functions used to check (`is.numeric`, `is.character`, ...)
#' @param msg list of messages to return in case of failed check



flux_fun_check <- function(args,
                           fn,
                           msg) {
  df_name <- if (is.data.frame(args)) deparse(substitute(args)) else NA
  mapply(flux_check_item, args, fn, msg, names(args), df_name = df_name)
}
# fn <- function(ar1 = 1, ar2 = 2, ar3 = 3, ar4 = "text", ar5 = data.frame(A = 1:3, B = LETTERS[1:3])){
#   args_ok <- flux_fun_check(list(ar1 = ar1, ar2 = ar2, ar3 = ar3, ar4 = ar4),
#     fn = list(is.numeric, is.numeric, is.numeric, is.character),
#     msg = rep(c("has to be numeric", "has to be character"), c(3,1)))
#   dataframe_ok <- flux_fun_check(ar5,
#     fn = list(is.numeric, is.character),
#     msg = c("has to be numeric", "has to be character"))
#   if (any(!c(args_ok, dataframe_ok))) stop("Please correct the arguments", call. = FALSE)
#   # code
# }

# fn("3", ar2 = 2, ar4 = 3, ar5 = data.frame(A = 2, B = 2))
# fn(2)


# flux_fun_check <- function(df,
#                            col_numeric = c(),
#                            col_datetime = c(),
#                            arg_numeric = c()) {
#   type <- c()
#   for (i in seq_along(arg_numeric)) {
#     type[i] <- class(eval(as.symbol(arg_numeric[i])))
#   }
#   arg_df <- tibble(
#     name = ((arg_numeric)),
#     type = type,
#     supposed_type = "numeric"
#   )


#   check_df <- sapply(((df)), class) |>
#     lapply(`[[`, 1) |>
#     bind_rows() |>
#     pivot_longer(everything(), values_to = "type") |>
#     mutate(
#       supposed_type = case_when(
#         name %in% ((col_numeric)) ~ "numeric",
#         name %in% ((col_datetime)) ~ "POSIXct"
#       )
#     ) |>
#     drop_na("supposed_type")

#   check_df <- bind_rows(check_df, arg_df) |>
#     mutate(
#       stop_msg = case_when(
#         type != supposed_type ~ paste("\n", name, "has to be", supposed_type)
#       ),
#       stop_msg = as.character(stop_msg)
#     ) |>
#     drop_na("stop_msg")


#   stop_msg <- stringr::str_c(check_df$stop_msg)

#   if (length(stop_msg) > 0) {
#     stop(stop_msg)
#   }
# }
