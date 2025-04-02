

flux_quality_kappamax <- function(slopes_df,
                                  f_slope = f_slope,
                                  f_fit = f_fit,
                                  f_slope_lm = f_slope_lm,
                                  f_fit_lm = f_fit_lm,
                                  f_b = f_b,
                                  fit_type = c(),
                                  instr_error,
                                  name_df) {

  args_ok <- flux_fun_check(list(
    instr_error = instr_error
  ),
  fn = list(is.numeric),
  msg = "has to be numeric"
  )

  slopes_df_check <- slopes_df |>
    select(
      {{f_slope_lm}},
      {{f_fit_lm}}
    )

  df_ok <- flux_fun_check(slopes_df_check,
                          fn = list(
                            is.numeric,
                            is.numeric
                          ),
                          msg = rep(
                            "has to be numeric",
                            2
                          ),
                          name_df = name_df)


  if (any(!c(args_ok, df_ok)))
    stop("Please correct the arguments", call. = FALSE)

slopes_df <- slopes_df |>
    mutate(
        kappamax = {{f_slope_lm}} / instr_error,
            {{f_fit}} := case_when(
                abs({{f_b}}) <= kappamax ~ {{f_fit}},
                abs({{f_b}}) > kappamax ~ {{f_fit_lm}}
            ),
            {{f_slope}} := case_when(
                abs({{f_b}}) <= kappamax ~ {{f_slope}},
                abs({{f_b}}) > kappamax ~ {{f_slope_lm}}
            ),
            f_model = case_when(
                abs({{f_b}}) <= kappamax ~ fit_type,
                abs({{f_b}}) > kappamax ~ "linear"
            )
        )

attr(slopes_df, "kappamax") <- TRUE

# add a message summary of how many got replaced with lm

slopes_df

}