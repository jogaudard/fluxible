# averaging works

    Code
      output
    Output
      # A tibble: 6 x 6
        f_fluxID   PAR temp_soil f_slope_calc temp_air_ave  flux
           <dbl> <dbl>     <dbl>        <dbl>        <dbl> <dbl>
      1        1  1.95      10.8        1.56          7.31  95.6
      2        2  2.11      10.7        0.853         7.38  52.4
      3        3  2.04      10.7        0.303         7.46  18.6
      4        4  1.84      10.8        1.13          7.77  69.4
      5        5  1.66      10.6        1.46          7.71  89.9
      6        6  1.78      12.2        0.426         7.75  26.2

# keeping works

    Code
      flux_calc(slopes0, slope_col = "f_slope_tz", cols_keep = c("turfID", "type",
        "f_start"))
    Output
      # A tibble: 6 x 7
        turfID      type  f_start             f_fluxID f_slope_calc temp_air_ave  flux
        <chr>       <chr> <dttm>                 <dbl>        <dbl>        <dbl> <dbl>
      1 156 AN2C 1~ ER    2022-07-28 23:43:35        1        1.56          7.31  95.6
      2 74 WN2C 155 NEE   2022-07-28 23:47:22        2        0.853         7.38  52.4
      3 74 WN2C 155 ER    2022-07-28 23:52:10        3        0.303         7.46  18.6
      4 109 AN3C 1~ NEE   2022-07-28 23:59:32        4        1.13          7.77  69.4
      5 109 AN3C 1~ ER    2022-07-29 00:03:10        5        1.46          7.71  89.9
      6 29 WN3C 106 NEE   2022-07-29 00:06:35        6        0.426         7.75  26.2

# keeping and averaging work together

    Code
      flux_calc(slopes0, slope_col = "f_slope_tz", cols_keep = c("turfID", "type",
        "f_start"), cols_ave = c("PAR", "temp_soil"))
    Output
      # A tibble: 6 x 9
        f_fluxID   PAR temp_soil turfID       type  f_start             f_slope_calc
           <dbl> <dbl>     <dbl> <chr>        <chr> <dttm>                     <dbl>
      1        1  1.95      10.8 156 AN2C 156 ER    2022-07-28 23:43:35        1.56 
      2        2  2.11      10.7 74 WN2C 155  NEE   2022-07-28 23:47:22        0.853
      3        3  2.04      10.7 74 WN2C 155  ER    2022-07-28 23:52:10        0.303
      4        4  1.84      10.8 109 AN3C 109 NEE   2022-07-28 23:59:32        1.13 
      5        5  1.66      10.6 109 AN3C 109 ER    2022-07-29 00:03:10        1.46 
      6        6  1.78      12.2 29 WN3C 106  NEE   2022-07-29 00:06:35        0.426
      # i 2 more variables: temp_air_ave <dbl>, flux <dbl>

# fahrenheit conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_fahr",
        temp_air_unit = "fahrenheit")
    Output
      # A tibble: 6 x 4
        f_fluxID f_slope_calc temp_air_ave  flux
           <dbl>        <dbl>        <dbl> <dbl>
      1        1        1.56          45.2  95.6
      2        2        0.853         45.3  52.4
      3        3        0.303         45.4  18.6
      4        4        1.13          46.0  69.4
      5        5        1.46          45.9  89.9
      6        6        0.426         45.9  26.2

# kelvin conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_kelvin",
        temp_air_unit = "kelvin")
    Output
      # A tibble: 6 x 4
        f_fluxID f_slope_calc temp_air_ave  flux
           <dbl>        <dbl>        <dbl> <dbl>
      1        1        1.56          280.  95.6
      2        2        0.853         281.  52.4
      3        3        0.303         281.  18.6
      4        4        1.13          281.  69.4
      5        5        1.46          281.  89.9
      6        6        0.426         281.  26.2

# calculating fluxes on dataset with cuts filters out the cuts first

    Code
      flux_calc(slopes30_flag, slope_col = "f_slope_corr", cut_col = "f_cut",
        keep_filter = "keep")
    Output
      # A tibble: 6 x 4
        f_fluxID f_slope_calc temp_air_ave  flux
           <dbl>        <dbl>        <dbl> <dbl>
      1        1        0.775         7.29  47.7
      2        2        0.504         7.37  31.0
      3        3        0.337         7.45  20.7
      4        4        0.676         7.77  41.5
      5        5        1.12          7.70  68.7
      6        6        0.425         7.74  26.1

