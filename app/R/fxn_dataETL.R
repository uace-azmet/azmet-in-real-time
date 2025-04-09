#' `fxn_dataETL.R` Download AZMet 15-minute data, transform variables, and return to app
#' 
#' @return `dataETL` - Downloaded 15-minute data and transformed variables over previous 24 hours, tibble format


fxn_dataETL <- function() {
  idRetrievingData <- 
    shiny::showNotification(
      ui = "Retrieving the latest data . . .", 
      action = NULL, 
      duration = NULL, 
      closeButton = FALSE,
      id = "idRetrievingData",
      type = "message"
    )
  
  dataETL <- 
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
    
    dplyr::select(
      meta_station_name,
      datetime,
      #meta_bat_volt,
      precip_total_in,
      relative_humidity,
      sol_rad_Wm2,
      temp_airF,
      dwptF,
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
  
  return(dataETL)
}
