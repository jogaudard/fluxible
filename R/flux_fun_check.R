

flux_fun_check <- function(df,
                           col_numeric = c(),
                           col_time = c(),
                           arg_numeric = c()) {
  type <- c()
for(i in 1:length(arg_numeric)) {
  type[i] <- class(eval(as.symbol(arg_numeric[i])))
}
  arg_df <- tibble(
    name = ((arg_numeric)),
    type = type,
    supposed_type = "numeric"
  )


  check_df <- sapply(((df)), class) |>
    lapply(`[[`, 1) |>
    bind_rows() |>
    pivot_longer(everything(), values_to = "type") |>
    mutate(
      supposed_type = case_when(
        name %in% ((col_numeric)) ~ "numeric",
        name %in% ((col_time)) ~ "POSIXct"
      )
    ) |>
    drop_na(supposed_type)

  check_df <- bind_rows(check_df, arg_df) |>
    mutate(
      stop_msg = case_when(
        type != supposed_type ~ paste("\n", name, "has to be", supposed_type)
      ),
      stop_msg = as.character(stop_msg)
    ) |>
    drop_na(stop_msg)


stop_msg <- stringr::str_c(check_df$stop_msg)

if(length(stop_msg) > 0) {stop(stop_msg)}


}