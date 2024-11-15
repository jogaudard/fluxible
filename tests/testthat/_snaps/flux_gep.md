# GEP calculation

    Code
      flux_gep(co2_fluxes, id_cols = "turfID", flux_col = "flux", type_col = "type",
        datetime_col = "f_start", par_col = "PAR", cols_keep = c())
    Condition
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"type"` instead of `.data$type`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"flux"` instead of `.data$flux`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"datetime"` instead of `.data$datetime`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"PAR"` instead of `.data$PAR`
      Warning:
      Specifying the `id_cols` argument by position was deprecated in tidyr 1.3.0.
      i Please explicitly name `id_cols`, like `id_cols = all_of(((id_cols)))`.
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"type"` instead of `.data$type`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"flux"` instead of `.data$flux`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"datetime"` instead of `.data$datetime`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"PAR"` instead of `.data$PAR`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"datetime"` instead of `.data$datetime`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"PAR"` instead of `.data$PAR`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"type"` instead of `.data$type`
      Warning:
      Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"flux"` instead of `.data$flux`
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
      Warning:
      Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      i See details at <https://tidyselect.r-lib.org/reference/faq-selection-context.html>
    Output
      # A tibble: 10 x 11
         datetime            turfID   PAR type   flux f_fluxID f_slope_tz temp_air_ave
         <dttm>              <fct>  <dbl> <chr> <dbl> <fct>         <dbl>        <dbl>
       1 NA                  156 A~ NA    GEP    NA   <NA>         NA            NA   
       2 2022-07-28 23:47:22 74 WN~  2.11 GEP    33.8 <NA>         NA            NA   
       3 2022-07-28 23:59:32 109 A~  1.84 GEP   -20.5 <NA>         NA            NA   
       4 2022-07-29 00:06:35 29 WN~  1.78 GEP    NA   <NA>         NA            NA   
       5 2022-07-28 23:43:35 156 A~  1.95 ER     95.6 1             1.56          7.31
       6 2022-07-28 23:47:22 74 WN~  2.11 NEE    52.4 2             0.853         7.38
       7 2022-07-28 23:52:10 74 WN~  2.04 ER     18.6 3             0.303         7.46
       8 2022-07-28 23:59:32 109 A~  1.84 NEE    69.4 4             1.13          7.77
       9 2022-07-29 00:03:10 109 A~  1.66 ER     89.9 5             1.46          7.71
      10 2022-07-29 00:06:35 29 WN~  1.78 NEE    26.2 6             0.426         7.75
      # i 3 more variables: temp_soil <dbl>, temp_fahr <dbl>, temp_kelvin <dbl>

