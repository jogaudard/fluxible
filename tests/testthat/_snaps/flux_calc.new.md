# averaging works

    Code
      output
    Output
      # A tibble: 6 x 13
        f_fluxID   PAR temp_soil f_slope_calc chamber_volume tube_volume plot_area
           <dbl> <dbl>     <dbl>        <dbl>          <dbl>       <dbl>     <dbl>
      1        1  1.95      10.8        1.56            24.5       0.075    0.0625
      2        2  2.11      10.7        0.853           24.5       0.075    0.0625
      3        3  2.04      10.7        0.303           24.5       0.075    0.0625
      4        4  1.84      10.8        1.13            24.5       0.075    0.0625
      5        5  1.66      10.6        1.46            24.5       0.075    0.0625
      6        6  1.78      12.2        0.426           24.5       0.075    0.0625
      # i 6 more variables: temp_air_avg <dbl>, atm_pressure_avg <dbl>,
      #   datetime <dttm>, volume_setup <dbl>, flux <dbl>, model <chr>

# keeping works

    Code
      flux_calc(slopes0, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        cols_keep = c("turfID", "type", "f_start"))
    Message
      Averaging air temperature and pressure for each flux...
      Creating a df with the columns from 'cols_keep' argument...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 14
        turfID       type  f_start             f_fluxID f_slope_calc chamber_volume
        <chr>        <chr> <dttm>                 <dbl>        <dbl>          <dbl>
      1 156 AN2C 156 ER    2022-07-28 23:43:35        1        1.56            24.5
      2 74 WN2C 155  NEE   2022-07-28 23:47:22        2        0.853           24.5
      3 74 WN2C 155  ER    2022-07-28 23:52:10        3        0.303           24.5
      4 109 AN3C 109 NEE   2022-07-28 23:59:32        4        1.13            24.5
      5 109 AN3C 109 ER    2022-07-29 00:03:10        5        1.46            24.5
      6 29 WN3C 106  NEE   2022-07-29 00:06:35        6        0.426           24.5
      # i 8 more variables: tube_volume <dbl>, plot_area <dbl>, temp_air_avg <dbl>,
      #   atm_pressure_avg <dbl>, datetime <dttm>, volume_setup <dbl>, flux <dbl>,
      #   model <chr>

# keeping and averaging work together

    Code
      flux_calc(slopes0, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        cols_keep = c("turfID", "type", "f_start"), cols_avg = c("PAR", "temp_soil"))
    Message
      Averaging air temperature and pressure for each flux...
      Creating a df with the columns from 'cols_keep' argument...
      Creating a df with the columns from 'cols_avg' argument...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 16
        f_fluxID   PAR temp_soil turfID       type  f_start             f_slope_calc
           <dbl> <dbl>     <dbl> <chr>        <chr> <dttm>                     <dbl>
      1        1  1.95      10.8 156 AN2C 156 ER    2022-07-28 23:43:35        1.56 
      2        2  2.11      10.7 74 WN2C 155  NEE   2022-07-28 23:47:22        0.853
      3        3  2.04      10.7 74 WN2C 155  ER    2022-07-28 23:52:10        0.303
      4        4  1.84      10.8 109 AN3C 109 NEE   2022-07-28 23:59:32        1.13 
      5        5  1.66      10.6 109 AN3C 109 ER    2022-07-29 00:03:10        1.46 
      6        6  1.78      12.2 29 WN3C 106  NEE   2022-07-29 00:06:35        0.426
      # i 9 more variables: chamber_volume <dbl>, tube_volume <dbl>, plot_area <dbl>,
      #   temp_air_avg <dbl>, atm_pressure_avg <dbl>, datetime <dttm>,
      #   volume_setup <dbl>, flux <dbl>, model <chr>

# fahrenheit conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        temp_air_col = "temp_fahr", temp_air_unit = "fahrenheit")
    Message
      Averaging air temperature and pressure for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 11
        f_fluxID f_slope_calc chamber_volume tube_volume plot_area temp_air_avg
           <dbl>        <dbl>          <dbl>       <dbl>     <dbl>        <dbl>
      1        1        1.56            24.5       0.075    0.0625         45.2
      2        2        0.853           24.5       0.075    0.0625         45.3
      3        3        0.303           24.5       0.075    0.0625         45.4
      4        4        1.13            24.5       0.075    0.0625         46.0
      5        5        1.46            24.5       0.075    0.0625         45.9
      6        6        0.426           24.5       0.075    0.0625         45.9
      # i 5 more variables: atm_pressure_avg <dbl>, datetime <dttm>,
      #   volume_setup <dbl>, flux <dbl>, model <chr>

# kelvin conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        temp_air_col = "temp_kelvin", temp_air_unit = "kelvin")
    Message
      Averaging air temperature and pressure for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 11
        f_fluxID f_slope_calc chamber_volume tube_volume plot_area temp_air_avg
           <dbl>        <dbl>          <dbl>       <dbl>     <dbl>        <dbl>
      1        1        1.56            24.5       0.075    0.0625         280.
      2        2        0.853           24.5       0.075    0.0625         281.
      3        3        0.303           24.5       0.075    0.0625         281.
      4        4        1.13            24.5       0.075    0.0625         281.
      5        5        1.46            24.5       0.075    0.0625         281.
      6        6        0.426           24.5       0.075    0.0625         281.
      # i 5 more variables: atm_pressure_avg <dbl>, datetime <dttm>,
      #   volume_setup <dbl>, flux <dbl>, model <chr>

# calculating fluxes on dataset with cuts

    Code
      dplyr::select(flux_calc(slopes30_flag, slope_col = "f_slope_corr", conc_unit = "ppm",
        flux_unit = "mmol", cut_col = "f_cut", keep_arg = "keep"), f_fluxID, flux,
      temp_air_avg, volume_setup)
    Message
      Cutting data according to 'keep_arg'...
      Averaging air temperature and pressure for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 4
        f_fluxID  flux temp_air_avg volume_setup
           <dbl> <dbl>        <dbl>        <dbl>
      1        1  47.7         7.29         24.6
      2        2  31.0         7.37         24.6
      3        3  20.7         7.45         24.6
      4        4  41.5         7.77         24.6
      5        5   0           7.70         24.6
      6        6  26.1         7.74         24.6

# volume can be a variable instead of a constant

    Code
      dplyr::select(flux_calc(slopes0_vol, slope_col = "f_slope_tz", conc_unit = "ppm",
        flux_unit = "mmol", chamber_volume = "volume"), f_fluxID, flux, volume_setup,
      temp_air_avg)
    Message
      Averaging air temperature and pressure for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 4
        f_fluxID  flux volume_setup temp_air_avg
           <dbl> <dbl>        <dbl>        <dbl>
      1        1  70.4        18.1          7.31
      2        2  59.9        28.1          7.38
      3        3  15.2        20.1          7.46
      4        4  68.0        24.1          7.77
      5        5  14.9         4.08         7.71
      6        6  37.3        35.1          7.75

# volume can be a variable instead of a constant (tube_volume)

    Code
      dplyr::select(flux_calc(slopes0_vol_tube, slope_col = "f_slope_tz", conc_unit = "ppm",
        flux_unit = "mmol", chamber_volume = "volume", tube_volume = "tube_vol"),
      f_fluxID, flux, volume_setup, temp_air_avg)
    Message
      Averaging air temperature and pressure for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 4
        f_fluxID  flux volume_setup temp_air_avg
           <dbl> <dbl>        <dbl>        <dbl>
      1        1  74.0         19           7.31
      2        2  68.3         32           7.38
      3        3  15.3         20.2         7.46
      4        4  68.0         24.1         7.77
      5        5  21.9          6           7.71
      6        6  37.8         35.5         7.75

# Fluxible workflow works from start to finish

    Code
      str(fluxes_test)
    Output
      tibble [6 x 11] (S3: tbl_df/tbl/data.frame)
       $ f_fluxID        : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6
       $ f_slope_calc    : num [1:6] 0 0 0.303 0 0 ...
       $ chamber_volume  : num [1:6] 24.5 24.5 24.5 24.5 24.5 24.5
       $ tube_volume     : num [1:6] 0.075 0.075 0.075 0.075 0.075 0.075
       $ plot_area       : num [1:6] 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625
       $ temp_air_avg    : num [1:6] 7.31 7.38 7.46 7.77 7.71 ...
       $ atm_pressure_avg: num [1:6] 1 1 1 1 1 1
       $ datetime        : POSIXct[1:6], format: "2022-07-28 23:43:35" "2022-07-28 23:47:22" ...
       $ volume_setup    : num [1:6] 24.6 24.6 24.6 24.6 24.6 ...
       $ flux            : num [1:6] 0 0 18.6 0 0 ...
       $ model           : chr [1:6] "exponential" "exponential" "exponential" "exponential" ...

# flux_calc works with segmentation tool

    Code
      dplyr::select(flux_calc(slopes_pftc7_flags, slope_col = "f_mean_slope_corr",
        conc_unit = "ppm", flux_unit = "mmol", temp_air_col = "temperature_c",
        atm_pressure = "pressure", chamber_volume = 2197, tube_volume = 0, plot_area = 1.69),
      f_fluxID, volume_setup, temp_air_avg, plot_area, flux, model)
    Message
      Averaging air temperature and pressure for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 13 x 6
         f_fluxID                     volume_setup temp_air_avg plot_area   flux model
         <fct>                               <dbl>        <dbl>     <dbl>  <dbl> <chr>
       1 5_2800_east_1_day_photo.txt          2197         23.0      1.69   8.45 segm~
       2 5_2800_east_2_day_photo.txt          2197         23.0      1.69   5.81 segm~
       3 5_2800_east_3_day_photo.txt          2197         22.9      1.69   5.37 segm~
       4 5_2800_east_4_day_photo.txt          2197         22.2      1.69  NA    segm~
       5 5_2800_east_5_day_photo.txt          2197         19.7      1.69 -12.1  segm~
       6 5_2800_east_5_day_redo_phot~         2197         19.6      1.69 -12.4  segm~
       7 5_2800_west_1_day_photo.txt          2197         20.9      1.69 -13.6  segm~
       8 5_2800_west_2_day_photo.txt          2197         21.7      1.69  NA    segm~
       9 5_2800_west_2_day_redo_phot~         2197         22.0      1.69  -8.10 segm~
      10 5_2800_west_3_day_photo.txt          2197         21.2      1.69  NA    segm~
      11 5_2800_west_3_day_redo_phot~         2197         20.5      1.69  -6.05 segm~
      12 5_2800_west_4_day_redo_phot~         2197         20.9      1.69   6.31 segm~
      13 5_2800_west_5_day_photo.txt          2197         18.7      1.69  -3.76 segm~

