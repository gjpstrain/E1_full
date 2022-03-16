library(MASS)
library(tidyverse)
library(Hmisc)
library(modelr)
library(scales)

r_vec <- correlations$my_rs

set.seed(1234)
add_residual_function(0.2795924)

my_function <- function(my_desired_r) {

  my_sample_size = 50
  
  mean_variable_1 = 0
  sd_variable_1 = 1
  
  mean_variable_2 = 0
  sd_variable_2 = 1
  
  mu <- c(mean_variable_1, mean_variable_2) 
  
  myr <- my_desired_r * sqrt(sd_variable_1) * sqrt(sd_variable_2)
  
  mysigma <- matrix(c(sd_variable_1, myr, myr, sd_variable_2), 2, 2) 
  
  corr_data = as_tibble(mvrnorm(my_sample_size, mu, mysigma, empirical = TRUE))
  
  corr_model <- lm(V2 ~ V1, data = corr_data)
  
  my_residuals <- abs(residuals(corr_model))
  
  data_with_resid <- round(cbind(corr_data, my_residuals), 2)
  
  return(sum(data_with_resid$my_residuals))
  
}

for(value in r_vec) {
  new_value <- my_function(value)
  res_vec <- c(res_vec, new_value) 
}

full_1 <- cbind(correlations, res_vec)


