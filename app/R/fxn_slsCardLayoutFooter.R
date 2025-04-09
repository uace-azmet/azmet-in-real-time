#' `fxn_slsCardLayoutFooter.R` - Build footer for card layout on station-level summaries tab
#' 
#' @return `slsCardLayoutFooter` - Footer for card layout on station-level summaries tab


fxn_slsCardLayoutFooter <- function() {
  slsCardLayoutFooter <- 
    htmltools::p(
      htmltools::HTML(
        "<sup>1</sup> Since midnight local time&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Since the top of the hour<br><br>Variable key: <strong>P</strong><sup>1</sup> total precipitation in inches; <strong>RH</strong> relative humidity in percent; <strong>SR</strong> solar radiation in watts per square meter; <strong>T</strong> air temperature in degrees Fahrenheit; <strong>T<sub>max</sub></strong><sup>1</sup> maximum air temperature in degrees Fahrenheit; <strong>T<sub>min</sub></strong><sup>1</sup> minimum air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>T<sub>soil 4-inch</sub></strong> soil temperature at four inches depth in degrees Fahrenheit; <strong>T<sub>soil 20-inch</sub></strong> soil temperature at four inches depth in degrees Fahrenheit; <strong>WD</strong> wind vector direction in degrees; <strong>WD<sub>2-min</sub></strong> wind_2min_vector_dir; <strong>WD<sub>2-min max</sub></strong><sup>1</sup> wind vector direction of two-minute sustained wind speed in degrees; <strong>WD<sub>2-min max</sub></strong><sup>2</sup> wind vector direction of two-minute sustained wind speed in degrees; <strong>WS</strong> wind speed in miles per hour; <strong>WS<sub>max</sub></strong><sup>1</sup> maximum wind speed in miles per hour; <strong>WS<sub>2-min</sub></strong> two-minute sustained wind speed in miles per hour; <strong>WS<sub>2-min max</sub></strong><sup>1</sup> maximum two-minute sustained wind speed in miles per hour; <strong>WS<sub>2-min max</sub></strong><sup>2</sup> maximum two-minute sustained wind speed in miles per hour"
      ),
      
      class = "sls-card-layout-footer"
    )
  
  return(slsCardLayoutFooter)
}
