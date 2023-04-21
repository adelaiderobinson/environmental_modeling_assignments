### loading packages
library(tidyverse)
library(here)

s


### reading in the climate data
climate_data <- read.table(here("clim.txt"), header = TRUE)

### cleaning data


wy = wy_input
climate_data <- climate_data |> 
  filter

minT2 <- climate_data |> 
  filter(month == 2) |> 
  group_by(year) |> 
  summarize(min_tmin_c = min(tmin_c))

P1 <- climate_data |> 
  filter(month == 1) |> 
  group_by(year) |> 
  summarize(total_precip_mm = sum(precip))

input_table <- full_join(minT2, P1, by = "year")


### almond yield equation

yield <- (-0.015* minT2) - (0.0046 * (minT2)^2) - (0.07 * P1)+ (0.0043 * (P1)^2) + .28



### applying equation for each year with a for loop
climate_yrs <- unique(climate_data$year)

#defining years for testing
years <- c(1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)

# ESTABLISHING empty dataframe
yield_df <- data.frame(year = numeric(length(years)), yield = numeric(length(years)))

for (i in seq_along(years)){
  
  year <- years[i]
  minT2 <- input_table$min_tmin_c[i]
  P1 <- input_table$total_precip_mm[i]
  
  yield <- ((-0.015* minT2) - (0.0046 * (minT2)^2) - (.07 * P1) + (0.0043 * (P1)^2) + 0.28)
  
  yield_df[i, ] <- c(year, yield)
}
  

################### -------------------------------

### -----------------------
# TEST the args
### -----------------------

year <- 2012

almond_yield <- function(years, climate_data){
  
  ### -----------------------
  # cleaning the data
  ### -----------------------
  
  # filtering based on input years
  climate_data <- climate_data |> 
    filter(year %in% years)
  
  # calculating the minimum temp values for Feb,
  minT2 <- climate_data |> 
    filter(month == 2) |> 
    group_by(year) |> 
    summarize(min_tmin_c = min(tmin_c))
  
  # calculating the total precipitation values for Jan,
  P1 <- climate_data |> 
    filter(month == 1) |> 
    group_by(year) |> 
    summarize(total_precip_mm = sum(precip))
  
  # creating a table to pull formula inputs from
  input_table <- full_join(minT2, P1, by = "year")
  
  ### -----------------------
  # calculating almond yields
  ### -----------------------

  
  # establishing an empty dataframe to populate
  yield_df <- data.frame(year = numeric(length(years)), yield = numeric(length(years)))
  
  # looping through each year to calculate the yield for the year
  for (i in seq_along(years)){
    
    # formula inputs
    year <- years[i]
    minT2 <- input_table$min_tmin_c[i]
    P1 <- input_table$total_precip_mm[i]
    
    # formula to calculate yield
    yield <- ((-0.015* minT2) - (0.0046 * (minT2)^2) - (.07 * P1) + (0.0043 * (P1)^2) + 0.28)
    
    # populating the dataframe with yield and year values
    yield_df[i, ] <- c(year, yield)
  }
  
  # calculating the mean, min and max anamolies for given years
  min_anomaly <- min(yield_df$yield)
  max_anomaly <- max(yield_df$yield)
  avg_anomaly <- mean(yield_df$yield)
  
  anomaly_df <- tibble(anomaly = c("min_anomaly", "max_anomaly", "avg_anomaly"),
                       value = as.numeric(c(min_anomaly, max_anomaly, avg_anomaly)))
  
  return(anomaly_df)
  }


### inputs - filepath, crop,  year 

