library(scales)

x <- matrix(1:10, ncol = 1)

rescale_mid(x, to = c(0,1), mid = 2)


set.seed(1234)

my_sample_size = 128

my_desired_r = 0.8

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

scaled_residuals_low <- rescale_mid(data_with_resid$my_residuals, to = c(0,1), mid = 0.75)
scaled_residuals_high <- rescale_mid(data_with_resid$my_residuals, to = c(0,1), mid = 0.25)
scaled_residuals <- rescale_mid(data_with_resid$my_residuals, to = c(0,1), mid = 0.5)


scaled_data <- cbind(data_with_resid, scaled_residuals_low, scaled_residuals_high, scaled_residuals)

ggplot(scaled_data, aes(x = V1, y = V2)) +
  scale_alpha_identity() +
  geom_point(aes(alpha = 1-scaled_residuals_low))  +
  #geom_smooth(method = "lm", se = FALSE) +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.text = element_blank()) +
  theme(legend.position = "None")

