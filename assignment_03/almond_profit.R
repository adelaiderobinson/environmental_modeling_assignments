
#' Calculate profit from almond yield anomaly
#'
#' @param minT2 The minimum temperature in February in °C. The default value is 9.88°C, calculated based on average minimum temperatures in February from 1989 to 2010 provided in EDS 230.
#' @param P1 The total January rainfall in mm. The default value is 132.27 mm, calculated based on average total January rainfall from 1989 to 2010 - data provided in EDS 230. 
#' @param price_per_ton The wholesale price of almonds per ton in 2021 USD. The default value is $4000/ton, based on values in "Market Integration and Price Discovery in California’s Almond Marketing: A Vector Auto-Regressive Approach" by Xu, Lone, and Torres.
#' @param acres The size of the almond farm. Default parameter is 1 acre.
#' @param k The constant calculated from the statistical yield model constructed by Lobell et al.
#'
#' @return A dataframe containing the almond yield anomaly in tons/acre, the profit per acre, and the total profit
#'
almond_profit <- function(beta_t1 = -0.015,
                          beta_t2 = 0.0046,
                          beta_p1 = .07,
                          beta_p2 = 0.0043,
                          k = 0.28,
                          minT2 = 9.88,
                          P1 = 132.27,
                          price_per_ton = 4000,
                          acres = 1
                          ) {
  ### -----------------------
  # calculating almond yields
  ### -----------------------
  
  # formula to calculate yield
  yield_anomaly <- ((beta_t1* minT2) - (beta_t2 * (minT2)^2) - (beta_p1 * P1) + (beta_p2 * (P1)^2) + k)
  
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
  
  profit_acre_list <- c("profit per acre", profit_per_acre)
  profit_list <- c("profit", profit)
  beta_t1_list <- c("beta_t1", beta_t1)
  beta_t2_list <- c("beta_t2", beta_t2)
  yield_anomaly_list <- c("yield anomaly", yield_anomaly)
  
  return_list <- list(yield_anomaly_list, profit_acre_list, profit_list, beta_t1_list, beta_t2_list)
  
  return_df <- tibble(profit_per_acre, profit, beta_t1, beta_t2, yield_anomaly)
  
  return(return_df)
  
  }