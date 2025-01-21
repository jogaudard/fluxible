# averaging works

    Code
      output
    Output
      # A tibble: 6 x 13
        f_fluxID   PAR temp_soil f_slope_calc chamber_volume tube_volume atm_pressure
           <dbl> <dbl>     <dbl>        <dbl>          <dbl>       <dbl>        <dbl>
      1        1  1.95      10.8        1.56            24.5       0.075            1
      2        2  2.11      10.7        0.853           24.5       0.075            1
      3        3  2.04      10.7        0.303           24.5       0.075            1
      4        4  1.84      10.8        1.13            24.5       0.075            1
      5        5  1.66      10.6        1.46            24.5       0.075            1
      6        6  1.78      12.2        0.426           24.5       0.075            1
      # i 6 more variables: plot_area <dbl>, temp_air_ave <dbl>, datetime <dttm>,
      #   volume_setup <dbl>, flux <dbl>, model <chr>

# keeping works

    Code
      flux_calc(slopes0, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        cols_keep = c("turfID", "type", "f_start"))
    Message
      Averaging air temperature for each flux...
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
      # i 8 more variables: tube_volume <dbl>, atm_pressure <dbl>, plot_area <dbl>,
      #   temp_air_ave <dbl>, datetime <dttm>, volume_setup <dbl>, flux <dbl>,
      #   model <chr>

# keeping and averaging work together

    Code
      flux_calc(slopes0, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        cols_keep = c("turfID", "type", "f_start"), cols_ave = c("PAR", "temp_soil"))
    Message
      Averaging air temperature for each flux...
      Creating a df with the columns from 'cols_keep' argument...
      Creating a df with the columns from 'cols_ave' argument...
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
      # i 9 more variables: chamber_volume <dbl>, tube_volume <dbl>,
      #   atm_pressure <dbl>, plot_area <dbl>, temp_air_ave <dbl>, datetime <dttm>,
      #   volume_setup <dbl>, flux <dbl>, model <chr>

# fahrenheit conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        temp_air_col = "temp_fahr", temp_air_unit = "fahrenheit")
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 11
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure plot_area
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>     <dbl>
      1        1        1.56            24.5       0.075            1    0.0625
      2        2        0.853           24.5       0.075            1    0.0625
      3        3        0.303           24.5       0.075            1    0.0625
      4        4        1.13            24.5       0.075            1    0.0625
      5        5        1.46            24.5       0.075            1    0.0625
      6        6        0.426           24.5       0.075            1    0.0625
      # i 5 more variables: temp_air_ave <dbl>, datetime <dttm>, volume_setup <dbl>,
      #   flux <dbl>, model <chr>

# kelvin conversion works

    Code
      flux_calc(slopes0_temp, slope_col = "f_slope", conc_unit = "ppm", flux_unit = "mmol",
        temp_air_col = "temp_kelvin", temp_air_unit = "kelvin")
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 11
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure plot_area
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>     <dbl>
      1        1        1.56            24.5       0.075            1    0.0625
      2        2        0.853           24.5       0.075            1    0.0625
      3        3        0.303           24.5       0.075            1    0.0625
      4        4        1.13            24.5       0.075            1    0.0625
      5        5        1.46            24.5       0.075            1    0.0625
      6        6        0.426           24.5       0.075            1    0.0625
      # i 5 more variables: temp_air_ave <dbl>, datetime <dttm>, volume_setup <dbl>,
      #   flux <dbl>, model <chr>

# calculating fluxes on dataset with cuts

    Code
      flux_calc(slopes30_flag, slope_col = "f_slope_corr", conc_unit = "ppm",
        flux_unit = "mmol", cut_col = "f_cut", keep_arg = "keep")
    Message
      Cutting data according to 'keep_arg'...
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 11
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure plot_area
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>     <dbl>
      1        1        0.775           24.5       0.075            1    0.0625
      2        2        0.504           24.5       0.075            1    0.0625
      3        3        0.337           24.5       0.075            1    0.0625
      4        4        0.676           24.5       0.075            1    0.0625
      5        5        1.12            24.5       0.075            1    0.0625
      6        6        0.425           24.5       0.075            1    0.0625
      # i 5 more variables: temp_air_ave <dbl>, datetime <dttm>, volume_setup <dbl>,
      #   flux <dbl>, model <chr>

# volume can be a variable instead of a constant

    Code
      flux_calc(slopes0_vol, slope_col = "f_slope_tz", conc_unit = "ppm", flux_unit = "mmol",
        chamber_volume = "volume")
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 10
        f_fluxID f_slope_calc chamber_volume tube_volume atm_pressure plot_area
           <dbl>        <dbl>          <dbl>       <dbl>        <dbl>     <dbl>
      1        1        1.56              18       0.075            1    0.0625
      2        2        0.853             28       0.075            1    0.0625
      3        3        0.303             20       0.075            1    0.0625
      4        4        1.13              24       0.075            1    0.0625
      5        5        1.46               4       0.075            1    0.0625
      6        6        0.426             35       0.075            1    0.0625
      # i 4 more variables: temp_air_ave <dbl>, datetime <dttm>, volume_setup <dbl>,
      #   flux <dbl>

# volume can be a variable instead of a constant (volume)

    Code
      select(flux_calc(slopes0_vol_tube, slope_col = "f_slope_tz", conc_unit = "ppm",
        flux_unit = "mmol", chamber_volume = "volume", tube_volume = "tube_vol"), !c(
        chamber_volume, tube_volume))
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 6 x 8
        f_fluxID f_slope_calc atm_pressure plot_area temp_air_ave datetime           
           <dbl>        <dbl>        <dbl>     <dbl>        <dbl> <dttm>             
      1        1        1.56             1    0.0625         7.31 2022-07-28 23:43:35
      2        2        0.853            1    0.0625         7.38 2022-07-28 23:47:22
      3        3        0.303            1    0.0625         7.46 2022-07-28 23:52:10
      4        4        1.13             1    0.0625         7.77 2022-07-28 23:59:32
      5        5        1.46             1    0.0625         7.71 2022-07-29 00:03:10
      6        6        0.426            1    0.0625         7.75 2022-07-29 00:06:35
      # i 2 more variables: volume_setup <dbl>, flux <dbl>

# Fluxible workflow works from start to finish

    Code
      str(fluxes_test)
    Output
      tibble [6 x 11] (S3: tbl_df/tbl/data.frame)
       $ f_fluxID      : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 4 5 6
       $ f_slope_calc  : num [1:6] 1.555 0.853 0.303 1.13 1.463 ...
       $ chamber_volume: num [1:6] 24.5 24.5 24.5 24.5 24.5 24.5
       $ tube_volume   : num [1:6] 0.075 0.075 0.075 0.075 0.075 0.075
       $ atm_pressure  : num [1:6] 1 1 1 1 1 1
       $ plot_area     : num [1:6] 0.0625 0.0625 0.0625 0.0625 0.0625 0.0625
       $ temp_air_ave  : num [1:6] 7.31 7.38 7.46 7.77 7.71 ...
       $ datetime      : POSIXct[1:6], format: "2022-07-28 23:43:35" "2022-07-28 23:47:22" ...
       $ volume_setup  : num [1:6] 24.6 24.6 24.6 24.6 24.6 ...
       $ flux          : num [1:6] 95.6 52.4 18.6 69.4 89.9 ...
       $ model         : chr [1:6] "exponential" "exponential" "exponential" "exponential" ...

# flux_calc works with segmentation tool

    Code
      dplyr::select(flux_calc(slopes_pftc7_flags, slope_col = "f_mean_slope_corr",
        conc_unit = "ppm", flux_unit = "mmol", temp_air_col = "temperature_c",
        chamber_volume = 1728, tube_volume = 0, plot_area = 1.44), f_fluxID,
      volume_setup, temp_air_ave, plot_area, flux, model)
    Message
      Averaging air temperature for each flux...
      Calculating fluxes...
      R constant set to 0.082057
      Concentration was measured in ppm
      Fluxes are in mmol/m2/h
    Output
      # A tibble: 13 x 6
         f_fluxID                     volume_setup temp_air_ave plot_area   flux model
         <fct>                               <dbl>        <dbl>     <dbl>  <dbl> <chr>
       1 5_2800_east_1_day_photo.txt          1728         23.0      1.44  10.8  segm~
       2 5_2800_east_2_day_photo.txt          1728         23.0      1.44   7.43 segm~
       3 5_2800_east_3_day_photo.txt          1728         22.9      1.44   6.87 segm~
       4 5_2800_east_4_day_photo.txt          1728         22.2      1.44  NA    segm~
       5 5_2800_east_5_day_photo.txt          1728         19.7      1.44 -15.5  segm~
       6 5_2800_east_5_day_redo_phot~         1728         19.6      1.44 -15.9  segm~
       7 5_2800_west_1_day_photo.txt          1728         20.9      1.44 -17.4  segm~
       8 5_2800_west_2_day_photo.txt          1728         21.7      1.44  NA    segm~
       9 5_2800_west_2_day_redo_phot~         1728         22.0      1.44 -10.4  segm~
      10 5_2800_west_3_day_photo.txt          1728         21.2      1.44  NA    segm~
      11 5_2800_west_3_day_redo_phot~         1728         20.5      1.44  -7.75 segm~
      12 5_2800_west_4_day_redo_phot~         1728         20.9      1.44   8.08 segm~
      13 5_2800_west_5_day_photo.txt          1728         18.7      1.44  -4.81 segm~

