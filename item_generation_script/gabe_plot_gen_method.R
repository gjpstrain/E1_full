library(MASS)
library(tidyverse)
library(Hmisc)
library(modelr)
library(scales)
library(grid)

# Change the variable below to what you want ####
# You can change the sample size, mean and sds of your two variables
# and you desired Pearson's r value which is the my_desired_r variable

set.seed(1234)

my_sample_size = 128

my_desired_r = 0.8

mean_variable_1 = 0
sd_variable_1 = 1

mean_variable_2 = 0
sd_variable_2 = 1

# Run the Below ####

mu <- c(mean_variable_1, mean_variable_2) 

myr <- my_desired_r * sqrt(sd_variable_1) * sqrt(sd_variable_2)

mysigma <- matrix(c(sd_variable_1, myr, myr, sd_variable_2), 2, 2) 

corr_data = as_tibble(mvrnorm(my_sample_size, mu, mysigma, empirical = TRUE))

# Building Linear Model for Contrast Encoding ####

corr_model <- lm(V2 ~ V1, data = corr_data)

my_residuals <- abs(residuals(corr_model))

data_with_resid <- round(cbind(corr_data, my_residuals), 2)

ggplot(data_with_resid, aes(x = V1, y = V2)) +
  geom_point(aes(alpha = -my_residuals))  +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.text = element_blank()) +
  theme(plot.margin = unit(c(2,2,2,2), "cm")) +
  theme(legend.position = "None")

sum(data_with_resid$my_residuals)

library(ggplot2)
library(grid)
qplot(rnorm(100)) +
  ggtitle("Title") +
  theme(axis.title.x=element_text(vjust=-2)) +
  theme(axis.title.y=element_text(angle=90, vjust=-0.5)) +
  theme(plot.title=element_text(size=15, vjust=3)) +
  theme(plot.margin = unit(c(10,10,10,10), "cm"))


