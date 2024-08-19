

flux_fun_check <- function(df,
                           col_numeric = c("f_datetime"),
                           col_time = c("temp_air"),
                           arg_numeric = c()) {
  arg_df <- tibble(
    name = ((arg_numeric)),
    type = class()
  )

  # maybe I make a for loop for the arguments??

  check_df <- sapply(slopes0, class) |>
    lapply(`[[`, 1) |>
    bind_rows() |>
    pivot_longer(everything(), values_to = "type") |>
    mutate(
      supposed_type = case_when(
        name %in% ((col_numeric)) ~ "numeric",
        name %in% ((col_time)) ~ "POSIXct"
      )
    ) |>
    drop_na(supposed_type) |>
    mutate(
      stop_msg = case_when(
        type != supposed_type ~ paste("\n", name, "has to be", supposed_type)
      ),
      stop_msg = as.character(stop_msg)
    )







# if(!is.na(((arg_double)))) {
# if (!is.double(((arg_double)))) arg_msg <- paste("\n", ((arg_double)), "has to be a double"))
# }

stop_msg <- stringr::str_c(check_df$stop_msg)

if(length(stop_msg) > 0) {stop(stop_msg)}


}