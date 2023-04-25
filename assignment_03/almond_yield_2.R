### -----------------------
# Function Documentation
### -----------------------

#' Calculating Almond Yield Based on Climate Data
#' 
#' Please load the tidyverse library to use this function 
#' 
#' 
#' @param years The years of interest with default years being 1989 to 2003. Please input years as a vector ex. c(1989, 1999, 2000).Note: years do not need to be consecutive.
#' @param climate_data This input takes the data as a dataframe with year, days, month, tmin_c (daily minimum temperature in degrees Celsius), and precip (the daily precipitation in millimeters.)
#'
#' @return A dataframe with minimum, maximum and average almond yield anomaly in ton/acres.
#' @export
#'
#' 
#' 


almond_yield <- function(minT2, P1){
  
  ### -----------------------
  # calculating almond yields
  ### -----------------------
  
    # formula to calculate yield
    yield <- ((-0.015* minT2) - (0.0046 * (minT2)^2) - (.07 * P1) + (0.0043 * (P1)^2) + 0.28)
    
    # populating the dataframe with yield and year values
    yield_df <- tibble(yield = as.numeric(c(yield)))
  }
  
  return(yiled_df)
}