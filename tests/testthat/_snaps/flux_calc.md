# averaging works

    Code
      output
    Output
      # A tibble: 6 x 8
        f_fluxID   PAR temp_soil f_slope_calc chamber_volume temp_air_ave volume_setup
           <dbl> <dbl>     <dbl>        <dbl>          <dbl>        <dbl>        <dbl>
      1        1  1.95      10.8        1.56            24.5         7.31         24.6
      2        2  2.11      10.7        0.853           24.5         7.38         24.6
      3        3  2.04      10.7        0.303           24.5         7.46         24.6
      4        4  1.84      10.8        1.13            24.5         7.77         24.6
      5        5  1.66      10.6        1.46            24.5         7.71         24.6
      6        6  1.78      12.2        0.426           24.5         7.75         24.6
      # i 1 more variable: flux <dbl>

# keeping works

    Code
      flux_calc(slopes0, slope_col = "f_slope_tz", cols_keep = c("turfID", "type",
        "f_start"))
    Output
      # A tibble: 6 x 9
        turfID       type  f_start             f_fluxID f_slope_calc chamber_volume
        <chr>        <chr> <dttm>                 <dbl>        <dbl>          <dbl>
      1 156 AN2C 156 ER    2022-07-28 23:43:35        1        1.56            24.5
      2 74 WN2C 155  NEE   2022-07-28 23:47:22        2        0.853           24.5
      3 74 WN2C 155  ER    2022-07-28 23:52:10        3        0.303           24.5
      4 109 AN3C 109 NEE   2022-07-28 23:59:32        4        1.13            24.5
      5 109 AN3C 109 ER    2022-07-29 00:03:10        5        1.46            24.5
      6 29 WN3C 106  NEE   2022-07-29 00:06:35        6        0.426           24.5
      # i 3 more variables: temp_air_ave <dbl>, volume_setup <dbl>, flux <dbl>

# keeping and averaging work together

    Code
      flux_calc(slopes0, slope_col = "f_slope_tz", cols_keep = c("turfID", "type",
        "f_start"), cols_ave = c("PAR", "temp_soil"))
    Output
      # A tibble: 6 x 11
        f_fluxID   PAR temp_soil turfID       type  f_start             f_slope_calc
           <dbl> <dbl>     <dbl> <chr>        <chr> <dttm>                     <dbl>
      1        1  1.95      10.8 156 AN2C 156 ER    2022-07-28 23:43:35        1.56 
      2        2  2.11      10.7 74 WN2C 155  NEE   2022-07-28 23:47:22        0.853
      3        3  2.04      10.7 74 WN2C 155  ER    2022-07-28 23:52:10        0.303
      4        4  1.84      10.8 109 AN3C 109 NEE   2022-07-28 23:59:32        1.13 
      5        5  1.66      10.6 109 AN3C 109 ER    2022-07-29 00:03:10        1.46 
      6        6  1.78      12.2 29 WN3C 106  NEE   2022-07-29 00:06:35        0.426
      # i 4 more variables: chamber_volume <dbl>, temp_air_ave <dbl>,
      #   volume_setup <dbl>, flux <dbl>

# fahrenheit conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_fahr",
        temp_air_unit = "fahrenheit")
    Output
      # A tibble: 6 x 6
        f_fluxID f_slope_calc chamber_volume temp_air_ave volume_setup  flux
           <dbl>        <dbl>          <dbl>        <dbl>        <dbl> <dbl>
      1        1        1.56            24.5         45.2         24.6  95.6
      2        2        0.853           24.5         45.3         24.6  52.4
      3        3        0.303           24.5         45.4         24.6  18.6
      4        4        1.13            24.5         46.0         24.6  69.4
      5        5        1.46            24.5         45.9         24.6  89.9
      6        6        0.426           24.5         45.9         24.6  26.2

# kelvin conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_kelvin",
        temp_air_unit = "kelvin")
    Output
      # A tibble: 6 x 6
        f_fluxID f_slope_calc chamber_volume temp_air_ave volume_setup  flux
           <dbl>        <dbl>          <dbl>        <dbl>        <dbl> <dbl>
      1        1        1.56            24.5         280.         24.6  95.6
      2        2        0.853           24.5         281.         24.6  52.4
      3        3        0.303           24.5         281.         24.6  18.6
      4        4        1.13            24.5         281.         24.6  69.4
      5        5        1.46            24.5         281.         24.6  89.9
      6        6        0.426           24.5         281.         24.6  26.2

# calculating fluxes on dataset with cuts filters out the cuts first

    Code
      flux_calc(slopes30_flag, slope_col = "f_slope_corr", cut_col = "f_cut",
        keep_filter = "keep")
    Output
      # A tibble: 6 x 6
        f_fluxID f_slope_calc chamber_volume temp_air_ave volume_setup  flux
           <dbl>        <dbl>          <dbl>        <dbl>        <dbl> <dbl>
      1        1        0.775           24.5         7.29         24.6  47.7
      2        2        0.504           24.5         7.37         24.6  31.0
      3        3        0.337           24.5         7.45         24.6  20.7
      4        4        0.676           24.5         7.77         24.6  41.5
      5        5        1.12            24.5         7.70         24.6  68.7
      6        6        0.425           24.5         7.74         24.6  26.1

# volume can be a variable instead of a constant, giving different fluxes

    Code
      flux_calc(slopes0_vol, slope_col = "f_slope_tz", chamber_volume = "volume")
    Output
      # A tibble: 6 x 6
        f_fluxID f_slope_calc chamber_volume temp_air_ave volume_setup  flux
           <dbl>        <dbl>          <dbl>        <dbl>        <dbl> <dbl>
      1        1        1.56              18         7.31        18.1   70.4
      2        2        0.853             28         7.38        28.1   59.9
      3        3        0.303             20         7.46        20.1   15.2
      4        4        1.13              24         7.77        24.1   68.0
      5        5        1.46               4         7.71         4.08  14.9
      6        6        0.426             35         7.75        35.1   37.3

