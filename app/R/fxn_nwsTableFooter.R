#' `fxn_nwsTableFooter.R` - Build footer for network-wide summary table
#' 
#' @return `nwsTableFooter` - Footer for network-wide summary table


fxn_nwsTableFooter <- function() {
  
  heatIndexEquationURL <- 
    a(
      "equation", 
      href="https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml",
      target="_blank"
    )
  
  nwsTableFooter <- 
    htmltools::p(
      htmltools::HTML(
        paste0(
          "<sup>1</sup> Since midnight local time&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<sup>2</sup> Since the top of the hour<br><br>Values of 'NA' denote no data. Variable key: <strong>P</strong><sup>1</sup> total precipitation in inches; <strong>RH</strong> relative humidity in percent; <strong>SR</strong> solar radiation in watts per square meter; <strong>T</strong> air temperature in degrees Fahrenheit; <strong>T<sub>dew point</sub></strong> dew point temperature in degrees Fahrenheit; <strong>T<sub>heat index</sub></strong> apparent or ", '"', "feels like", '"', " temperature in degrees Fahrenheit based on T, RH, and the National Weather Service ", heatIndexEquationURL, "; <strong>T<sub>max</sub></strong><sup>1</sup> maximum air temperature in degrees Fahrenheit; <strong>T<sub>min</sub></strong><sup>1</sup> minimum air temperature in degrees Fahrenheit; <strong>T<sub>soil 4-inch</sub></strong> soil temperature at four inches depth in degrees Fahrenheit; <strong>T<sub>soil 20-inch</sub></strong> soil temperature at four inches depth in degrees Fahrenheit; <strong>WD</strong> wind vector direction in degrees; <strong>WD<sub>2-min</sub></strong> wind vector direction of two-minute sustained wind speed in degrees; <strong>WD<sub>2-min max</sub></strong><sup>1</sup> wind vector direction of two-minute sustained wind speed in degrees; <strong>WD<sub>2-min max</sub></strong><sup>2</sup> wind vector direction of two-minute sustained wind speed in degrees; <strong>WS</strong> wind speed in miles per hour; <strong>WS<sub>max</sub></strong><sup>1</sup> maximum wind speed in miles per hour; <strong>WS<sub>2-min</sub></strong> two-minute sustained wind speed in miles per hour; <strong>WS<sub>2-min max</sub></strong><sup>1</sup> maximum two-minute sustained wind speed in miles per hour; <strong>WS<sub>2-min max</sub></strong><sup>2</sup> maximum two-minute sustained wind speed in miles per hour"
        )
      ),
      
      class = "nws-table-footer"
    )
  
  return(nwsTableFooter)
}
