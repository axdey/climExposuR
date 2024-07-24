### Writen by Chen Chen on 4/18/2024
### In this function I calculated the heat index based on the National Weather Service
### modified Rothfusz regression, including the adjustments
### https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
### I also calculated the apparent temperature using the Kalkstein and Valimont 1986 equation

### input could be Fahrenheit/Celsius, dew point temperature/relative humidity; 
### rh/dp transformation is based on August-Roche-Magnus approximation
### Default output is a data.frame of four columns: heat index in Fahrenheit/Celsius 
### and apparent temperature in Fahrenheit/Celsius, all replaced with air temperature when below 68 Fahrenheit
### The apparent temperature is weird when dew point temperature is below freezing thus not recommended
#' @name heat_index
#' @title Calculate heat index and apparent temperature
#' @param temperature Air temperature in Fahrenheit or Celsius
#' @param rh Relative humidity in percentage (0-100) or NA if using dew point
#' @param dewpoint Dew point temperature in Fahrenheit or Celsius or NA if using relative humidity
#' @param dp TRUE or FALSE; TRUE if dew point temperature is used
#' @param t_f TRUE or FALSE; TRUE if air temperature is provided in Fahrenheit
#' @param dp_f TRUE or FALSE; TRUE if dew point temperature is provided in Fahrenheit
#' @param rh_original TRUE or FALSE; TRUE if relative humidity ranges from 0 to 1 and will be transformed to a range of 0 to 100
#' @return A data.frame of heat index in Fahrenheit/Celsius and apparent temperature in Fahrenheit/Celsius
#' @import data.table dplyr

#' @export

heat_index <- function( 
  temperature, 
  rh, 
  dewpoint, 
  dp, 
  t_f, 
  dp_f, 
  rh_original 
) {
  ## get temperature in right unit
  if (t_f) {
    temperature_c <- (temperature - 32) * 5 / 9
  } else {
    temperature_c <- temperature
    temperature <- (temperature_c * 9 / 5) + 32
  }
  
  ## get relative humidity from dew point or wise versa: August-Roche-Magnus approximation
  a <- 17.62
  b <- 243.12
  if (dp) { ## transformation based on simplified equation (works for -45 to 60 celsius): https://www.npl.co.uk/resources/q-a/dew-point-and-relative-humidity
    if (dp_f) {
      dewpoint_c <- (dewpoint - 32) * 5 / 9
    } else {
      dewpoint_c <- dewpoint
      dewpoint <- (dewpoint_c * 9 / 5) + 32
    }
    rh <- 100 * exp(a * dewpoint_c / (b + dewpoint_c)) / exp(a * temperature_c / (b + temperature_c))
  } else {
    if (rh_original) {
      rh <- rh * 100
    }
    dewpoint_c <- (b*(log(rh/100) + a*temperature_c/(b+temperature_c))) / (a - log(rh/100) - a*temperature_c/(b+temperature_c))
    dewpoint <- (dewpoint_c * 9 / 5) + 32
  }

  
  ## calculate heat index using the NWS equation for fahrenheit based on the NWS equation: https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
  hi_sim <- 0.5 * (temperature + 61.0 + ((temperature-68.0)*1.2) + (rh*0.094))
  hi_full <- -42.379 + 2.04901523*temperature + 10.14333127*rh - .22475541*temperature*rh - .00683783*temperature*temperature - .05481717*rh*rh + .00122874*temperature*temperature*rh + .00085282*temperature*rh*rh - .00000199*temperature*temperature*rh*rh
  
  loc_lowrh <- which(rh < 13 & temperature > 80 & temperature < 112)
  lowrh_adjustement <- ((13-rh)/4)* sqrt((17-abs(temperature-95))/17)
  hi_full[loc_lowrh] <- hi_full[loc_lowrh] - lowrh_adjustement[loc_lowrh]
  
  loc_highrh <- which(rh > 85 & temperature < 87 & temperature > 80)
  highrh_adjustement <- ((rh-85)/10) * ((87-temperature)/5)
  hi_full[loc_highrh] <- hi_full[loc_highrh] + highrh_adjustement[loc_highrh]
  
  hi <- hi_sim
  loc <- which(hi>=80)
  hi[loc] <- hi_full[loc]
  hi_c <- (hi - 32) * 5 / 9
  
  ## Caldulate the apparent temperature using Table 1 of Kalkstein and Valimont 1986: https://ciesin.columbia.edu/docs/001-609/001-609.html
  ## apparent temperature would be outrageous when dew point temperature <0 celsius
  at <- -2.653 + 0.994 * temperature_c + 0.0153 * dewpoint_c * dewpoint_c 
  at_f <- (at * 9 / 5) + 32

  ## replace heat index <68 by air temperature--should use wind chill but don't have the wind data
  loc_68 <- which(temperature<68)
  hi_final <- hi
  hi_final[loc_68] <- temperature[loc_68]
  hi_final_c <- hi_c
  hi_final_c[loc_68] <- temperature_c[loc_68]
  at_final <- at_f
  at_final[loc_68] <- temperature[loc_68]
  at_final_c <- at
  at_final_c[loc_68] <- temperature_c[loc_68]
  
  ## test results--everything included
  # return(data.frame(hi=hi_final, at=at_final, hi=hi, hi_sim=hi_sim, hi_full=hi_full, at=at_f, temperature=temperature, dp=dewpoint, hi_c=hi_c, at_c=at_final_c, hi_c=hi_c, at_c=at, temperature_c = temperature_c, dp_c=dewpoint_c, rh=rh))
  ## results with cutpoint at 68--air temperature used when smaller than this
  return(data.frame(hi=hi_final, at=at_final, hi_c=hi_final_c, at_c=at_final_c))
  ## results without cutpoint at 68
  # return(data.frame(hi=hi, at=at_f, hi_c=hi_c, at_c=at))
  ## reporting one set of results (heat index in F with cut point in 68F)
  # return(hi=hi_final)
}