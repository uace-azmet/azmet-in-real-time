#' `fxn_az15min.R` Download AZMet 15-minute data, transform variables, and return to app
#' 
#' @return `az15min` - Downloaded 15-minute data and transformed variables over previous 24 hours, tibble format


fxn_az15min <- function() {
  
  idRetrievingData <- 
    shiny::showNotification(
      ui = "Retrieving the latest data . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idRetrievingData",
      type = "message"
    )
  
  az15min <- 
    azmetr::az_15min(
      start_date_time = 
        lubridate::now(tzone = "America/Phoenix") - lubridate::hours(24)
    ) |>
    
    dplyr::filter(meta_station_name != "Test") |>
    
    dplyr::mutate(
      datetime = format(datetime, format = "%Y-%m-%d %H:%M:%S"),
      dwptF = fxn_c_to_f(dwpt),
      precip_total_in = fxn_mm_to_in(precip_total_mm),
      sol_rad_Wm2 = fxn_kwm2_to_wm2(sol_rad_kWm2),
      temp_airF = fxn_c_to_f(temp_airC),
      temp_air_maxF = fxn_c_to_f(temp_air_maxC),
      temp_air_minF = fxn_c_to_f(temp_air_minC),
      #temp_panelF = fxn_c_to_f(temp_panelC),
      temp_soil_10cmF = fxn_c_to_f(temp_soil_10cmC),
      temp_soil_50cmF = fxn_c_to_f(temp_soil_50cmC),
      #temp_wetbulbF = fxn_c_to_f(temp_wetbulbC),
      wind_2min_spd_max_mph_daily = fxn_mps_to_mph(wind_2min_spd_max_mps_daily),
      wind_2min_spd_max_mph_hourly = fxn_mps_to_mph(wind_2min_spd_max_mps_hourly),
      wind_2min_spd_mean_mph = fxn_mps_to_mph(wind_2min_spd_mean_mps),
      wind_spd_max_mph = fxn_mps_to_mph(wind_spd_max_mps),
      wind_spd_mph = fxn_mps_to_mph(wind_spd_mps)
    ) |>
    
    # Calculate heat index, from https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
    dplyr::mutate( # Values consistent with Steadman's results
      temp_heat_indexF = 
        0.5 * (temp_airF + 61.0 + ((temp_airF - 68.0) * 1.2) + (relative_humidity * 0.094))
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      temp_air_heat_index_avgF =
        round(mean(c(temp_airF, temp_heat_indexF), na.rm = TRUE), digits = 1)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      temp_heat_indexF =
        dplyr::if_else( # Rothfusz regression equation 
          temp_air_heat_index_avgF >= 80.0,
          -42.379 + 2.04901523 * temp_airF + 10.14333127 * relative_humidity - 0.22475541 * temp_airF * relative_humidity - .00683783 * temp_airF * temp_airF - 0.05481717 * relative_humidity * relative_humidity + 0.00122874 * temp_airF * temp_airF * relative_humidity + 0.00085282 * temp_airF * relative_humidity * relative_humidity - 0.00000199 * temp_airF * temp_airF * relative_humidity * relative_humidity,
          temp_heat_indexF
        )
    ) |>
    dplyr::mutate(
      temp_heat_indexF = 
        dplyr::if_else( # Adjustment 1
          relative_humidity < 13 & temp_airF >= 80.0 & temp_airF <= 112.0,
          temp_heat_indexF - (((13 - relative_humidity) / 4) * sqrt((17 - abs(temp_airF - 95)) / 17)),
          temp_heat_indexF
        )
    ) |>
    dplyr::mutate(
      temp_heat_indexF =
        dplyr::if_else( # Adjustment 2
          relative_humidity > 85 & temp_airF >= 80.0 & temp_airF <= 87.0,
          temp_heat_indexF + (((relative_humidity - 85) / 10) * ((87 - temp_airF) / 5)),
          temp_heat_indexF
        )
    ) |>
    
    dplyr::select(
      meta_station_name,
      datetime,
      #meta_bat_volt,
      precip_total_in,
      relative_humidity,
      sol_rad_Wm2,
      temp_airF,
      dwptF,
      temp_heat_indexF,
      temp_air_maxF,
      temp_air_minF,
      #temp_panelF,
      #temp_wetbulbF,
      temp_soil_10cmF,
      temp_soil_50cmF,
      #vp_actual,
      #vp_deficit,
      #vp_saturation,
      wind_spd_mph,
      wind_vector_dir,
      wind_spd_max_mph,
      wind_2min_spd_mean_mph,
      wind_2min_vector_dir,
      wind_2min_spd_max_mph_daily,
      wind_2min_vector_dir_max_daily,
      wind_2min_spd_max_mph_hourly,
      wind_2min_vector_dir_max_hourly
    ) |>
    
    dplyr::mutate(
      dplyr::across(
        c(
          "relative_humidity",
          "wind_vector_dir",
          "wind_2min_vector_dir",
          "wind_2min_vector_dir_max_daily",
          "wind_2min_vector_dir_max_hourly"
        ),
        \(x) round(x, digits = 0)
      )
    ) |>
    
    dplyr::mutate(
      dplyr::across(
        c(
          "temp_airF",
          "dwptF",
          "temp_heat_indexF",
          "temp_air_maxF",
          "temp_air_minF",
          #"temp_panelF",
          #"temp_wetbulbF",
          "temp_soil_10cmF",
          "temp_soil_50cmF",
          "wind_spd_mph",
          "wind_spd_max_mph",
          "wind_2min_spd_mean_mph",
          "wind_2min_spd_max_mph_daily",
          "wind_2min_spd_max_mph_hourly"
        ),
        \(x) round(x, digits = 1)
      )
    ) |>
    
    dplyr::mutate(
      dplyr::across(
        c(
          #"meta_bat_volt",
          "precip_total_in",
          "sol_rad_Wm2"#,
          #"vp_saturation",
          #"vp_actual",
          #"vp_deficit"
        ),
        \(x) round(x, digits = 2)
      )
    ) |>
    
    dplyr::arrange(meta_station_name)
  
  on.exit(shiny::removeNotification(id = idRetrievingData), add = TRUE)
  
  return(az15min)
}
