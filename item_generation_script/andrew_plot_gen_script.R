library(MASS)
library(tidyverse)
library(Hmisc)
library(modelr)
library(scales)

set.seed(1234)

# Define Variables ####

my_sample_size = 50

my_desired_r = .8

mean_variable_1 = 50
sd_variable_1 = 50

mean_variable_2 = 50
sd_variable_2 = 50

# Build Matrix ####

mu <- c(mean_variable_1, mean_variable_2) 

myr <- my_desired_r * sqrt(sd_variable_1) * sqrt(sd_variable_2)

mysigma <- matrix(c(sd_variable_1, myr, myr, sd_variable_2), 2, 2) 

corr_data = as_tibble(mvrnorm(my_sample_size, mu, mysigma, empirical = TRUE))

# Build Model ####

corr_model <- lm(V1 ~ V2, data = corr_data)

my_residuals <- residuals(corr_model)

alt_corr_data <- cbind(corr_data, my_scaled_residuals = rescale(my_residuals, to = c(0, 1)))

alt_corr_data %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point(alpha = 1 - (my_residuals)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()