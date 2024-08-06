# averaging works

    Code
      output
    Output
      # A tibble: 6 x 10
        f_fluxID   PAR temp_soil f_slope_calc chamber_volume tube_volume atm_pressure
           <dbl> <dbl>     <dbl>        <dbl>          <dbl>       <dbl>        <dbl>
      1        1  1.95      10.8        1.56            24.5       0.075            1
      2        2  2.11      10.7        0.853           24.5       0.075            1
      3        3  2.04      10.7        0.303           24.5       0.075            1
      4        4  1.84      10.8        1.13            24.5       0.075            1
      5        5  1.66      10.6        1.46            24.5       0.075            1
      6        6  1.78      12.2        0.426           24.5       0.075            1
      # i 3 more variables: temp_air_ave <dbl>, volume_setup <dbl>, flux <dbl>

# keeping works

    Code
      flux_calc(slopes0, slope_col = "f_slope_tz", cols_keep = c("turfID", "type",
        "f_start"))
    Output
      # A tibble: 6 x 11
        turfID       type  f_start             f_fluxID f_slope_calc chamber_volume
        <chr>        <chr> <dttm>                 <dbl>        <dbl>          <dbl>
      1 156 AN2C 156 ER    2022-07-28 23:43:35        1        1.56            24.5
      2 74 WN2C 155  NEE   2022-07-28 23:47:22        2        0.853           24.5
      3 74 WN2C 155  ER    2022-07-28 23:52:10        3        0.303           24.5
      4 109 AN3C 109 NEE   2022-07-28 23:59:32        4        1.13            24.5
      5 109 AN3C 109 ER    2022-07-29 00:03:10        5        1.46            24.5
      6 29 WN3C 106  NEE   2022-07-29 00:06:35        6        0.426           24.5
      # i 5 more variables: tube_volume <dbl>, atm_pressure <dbl>,
      #   temp_air_ave <dbl>, volume_setup <dbl>, flux <dbl>

# keeping and averaging work together

    Code
      flux_calc(slopes0, slope_col = "f_slope_tz", cols_keep = c("turfID", "type",
        "f_start"), cols_ave = c("PAR", "temp_soil"))
    Output
      # A tibble: 6 x 13
        f_fluxID   PAR temp_soil turfID       type  f_start             f_slope_calc
           <dbl> <dbl>     <dbl> <chr>        <chr> <dttm>                     <dbl>
      1        1  1.95      10.8 156 AN2C 156 ER    2022-07-28 23:43:35        1.56 
      2        2  2.11      10.7 74 WN2C 155  NEE   2022-07-28 23:47:22        0.853
      3        3  2.04      10.7 74 WN2C 155  ER    2022-07-28 23:52:10        0.303
      4        4  1.84      10.8 109 AN3C 109 NEE   2022-07-28 23:59:32        1.13 
      5        5  1.66      10.6 109 AN3C 109 ER    2022-07-29 00:03:10        1.46 
      6        6  1.78      12.2 29 WN3C 106  NEE   2022-07-29 00:06:35        0.426
      # i 6 more variables: chamber_volume <dbl>, tube_volume <dbl>,
      #   atm_pressure <dbl>, temp_air_ave <dbl>, volume_setup <dbl>, flux <dbl>

# fahrenheit conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_fahr",
        temp_air_unit = "fahrenheit")
    Output
      # A tibble: 6 x 8
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        1.56            24.5       0.075            1         45.2
      2        2        0.853           24.5       0.075            1         45.3
      3        3        0.303           24.5       0.075            1         45.4
      4        4        1.13            24.5       0.075            1         46.0
      5        5        1.46            24.5       0.075            1         45.9
      6        6        0.426           24.5       0.075            1         45.9
      # i 2 more variables: volume_setup <dbl>, flux <dbl>

# kelvin conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope_tz", temp_air_col = "temp_kelvin",
        temp_air_unit = "kelvin")
    Output
      # A tibble: 6 x 8
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        1.56            24.5       0.075            1         280.
      2        2        0.853           24.5       0.075            1         281.
      3        3        0.303           24.5       0.075            1         281.
      4        4        1.13            24.5       0.075            1         281.
      5        5        1.46            24.5       0.075            1         281.
      6        6        0.426           24.5       0.075            1         281.
      # i 2 more variables: volume_setup <dbl>, flux <dbl>

# calculating fluxes on dataset with cuts filters out the cuts first

    Code
      flux_calc(slopes30_flag, slope_col = "f_slope_corr", cut_col = "f_cut",
        keep_arg = "keep")
    Output
      # A tibble: 6 x 8
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        0.775           24.5       0.075            1         7.29
      2        2        0.504           24.5       0.075            1         7.37
      3        3        0.337           24.5       0.075            1         7.45
      4        4        0.676           24.5       0.075            1         7.77
      5        5        1.12            24.5       0.075            1         7.70
      6        6        0.425           24.5       0.075            1         7.74
      # i 2 more variables: volume_setup <dbl>, flux <dbl>

# volume can be a variable instead of a constant, giving different fluxes

    Code
      flux_calc(slopes0_vol, slope_col = "f_slope_tz", chamber_volume = "volume")
    Output
      # A tibble: 6 x 8
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure temp_air_ave
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>        <dbl>
      1        1        1.56              18       0.075            1         7.31
      2        2        0.853             28       0.075            1         7.38
      3        3        0.303             20       0.075            1         7.46
      4        4        1.13              24       0.075            1         7.77
      5        5        1.46               4       0.075            1         7.71
      6        6        0.426             35       0.075            1         7.75
      # i 2 more variables: volume_setup <dbl>, flux <dbl>

# volume can be a variable instead of a constant, giving different fluxes (testing the tubes)

    Code
      select(flux_calc(slopes0_vol_tube, slope_col = "f_slope_tz", chamber_volume = "volume",
        tube_volume = "tube_vol"), !c(chamber_volume, tube_volume))
    Output
      # A tibble: 6 x 6
        f_fluxID f_slope_calc atm_pressure temp_air_ave volume_setup  flux
           <dbl>        <dbl>        <dbl>        <dbl>        <dbl> <dbl>
      1        1        1.56             1         7.31         19    74.0
      2        2        0.853            1         7.38         32    68.3
      3        3        0.303            1         7.46         20.2  15.3
      4        4        1.13             1         7.77         24.1  68.0
      5        5        1.46             1         7.71          6    21.9
      6        6        0.426            1         7.75         35.5  37.8

