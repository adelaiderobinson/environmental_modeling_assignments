### loading packages
library(tidyverse)
library(here)


### reading in the climate data
climate_data <- read.table(here("clim.txt"), header = TRUE)

### cleaning data


wy = wy_input
climate_data <- climate_data |> 
  filter

minT2 <- climate_data |> 
  filter(month == 2) |> 
  group_by(year) |> 
  summarize(avg_tmin_c = mean(tmin_c))

P1 <- climate_data |> 
  filter(month == 1) |> 
  group_by(year) |> 
  summarize(avg_precip_mm = mean(precip))

input_table <- full_join(minT2, P1, by = "year")


### almond yield equation

yield <- (-0.015* minT2) - (0.0046 * (minT2)^2) - (0.07 * P1)+ (0.0043 * (P1)^2) + .28



### applying equation for each year with a for loop
climate_wys <- unique(climate_data$year)

#defining years for testing
years <- c(climate_wys)

# ESTABLISHING LIST
yields <- list()

for (i in seq_along(years)){
  
  year <- years[i]
  minT2 <- input_table$avg_tmin_c[i]
  P1 <- input_table$avg_precip_mm[i]
  
  yield <- (-0.015* minT2) - (0.0046 * (minT2)^2) - (.07 * P1) + (0.0043 * (P1)^2) + .28
  
  
  #minT2 <- input_table[i, ]
  yields[[i]] <- yield 
}
  
input_table[ 1,]
yield_1995 <- (-.015* 12.6) - (.0046 * (12.6)^2) - (.07 * 21.8) + (.0043 * (21.8)^2) + .28


### inputs - filepath, crop,  year 

