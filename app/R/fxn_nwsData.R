#' `fxn_nwsData.R` - Filter 15-minute data for the most recent report from each station
#' 
#' @param inData - AZMet 15-minute data from `fxn_dataETL.R`
#' @return `nwsData` - Most recent data from each station, tibble format


fxn_nwsData <- function(inData) {
  nwsData <- inData |>
    dplyr::select(
      meta_station_group,
      meta_station_name,
      datetime,
      #meta_bat_volt,
      precip_total_in,
      relative_humidity,
      sol_rad_Wm2,
      temp_airF,
      temp_air_maxF,
      temp_air_minF,
      #temp_panelF,
      dwptF,
      #temp_wetbulbF,
      temp_soil_10cmF,
      temp_soil_50cmF,
      #vp_actual,
      #vp_deficit,
      #vp_saturation,
      wind_vector_dir,
      wind_2min_vector_dir,
      wind_2min_vector_dir_max_daily,
      wind_2min_vector_dir_max_hourly,
      wind_spd_mph,
      wind_spd_max_mph,
      wind_2min_spd_mean_mph,
      wind_2min_spd_max_mph_daily,
      wind_2min_spd_max_mph_hourly
    )   |>
    
    dplyr::group_by(meta_station_name) |>
    dplyr::filter(datetime == max(datetime)) |>
    dplyr::ungroup() |>
    dplyr::select(!meta_station_group)
    
  return(nwsData)
}
