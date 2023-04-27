
almond_profit <- function(acres = 1, price_per_ton = 4000, minT2, P1) {
  ### -----------------------
  # calculating almond yields
  ### -----------------------
  
  # formula to calculate yield
  yield_anomaly <- ((-0.015* minT2) - (0.0046 * (minT2)^2) - (.07 * P1) + (0.0043 * (P1)^2) + 0.28)
  
  # populating the dataframe with yield and year values
  yield_anomaly_df <- tibble(yield = as.numeric(c(yield_anomaly)))
  
  ### -----------------------
  # calculating profit
  ### -----------------------
  
  avg_yield <- 0.9 #tons per acre
  
  profit_per_acre <- (yield_anomaly * price_per_ton) + (avg_yield * price_per_ton)
  
  profit <- profit_per_acre * acres
  
  ### -----------------------
  # return results
  ### -----------------------
  
  profit_df <- tibble(profit_per_acre = as.numeric(profit_per_acre),
                      profit = as.numeric(profit)) |> 
    cbind(yield_anomaly)
  
  return(profit_df)
  
  }