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
years <- c(climate_yrs)

# ESTABLISHING empty dataframe
yield_df <- data.frame(year = numeric(length(years)), yield = numeric(length(years)))

for (i in seq_along(years)){
  
  year <- years[i]
  minT2 <- input_table$min_tmin_c[i]
  P1 <- input_table$total_precip_mm[i]
  
  yield <- ((-0.015* minT2) - (0.0046 * (minT2)^2) - (.07 * P1) + (0.0043 * (P1)^2) + 0.28)
  
  yield_df[i, ] <- c(year, yield)
}
  



input_table[ 15,]
yield_1991 <- (-0.015 * 10.4) - (0.0046 * (10.4)^2) - (.07 * 135) + (0.0043 * (135)^2) + .28


### inputs - filepath, crop,  year 

