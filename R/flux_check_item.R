#' check the items inside `flux_fun_check`
#' @param 


flux_check_item <- function(arg,
                            fn,
                            msg,
                            narg,
                            df_name = NA) {
    isok <- fn(arg)
    if (!isok) {
      msg_parts <- if (!is.na(df_name)) c("Column ", paste0(" of data frame ", df_name, " ")) else c("Argument ", " ")
      message(paste0(msg_parts[1], narg, msg_parts[2], msg))
      FALSE
    } else TRUE
  }