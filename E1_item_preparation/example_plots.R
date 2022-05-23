dir.create("example_plots")

set.seed(1234)

my_desired_r = 0.95

my_sample_size = 128

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

test_plot <- ggplot(data_with_resid, aes(x = V1, y = V2)) +
  geom_point()  +
  labs(x = "", y = "") +
  theme_classic() +
  theme(axis.text = element_blank()) +
  theme(plot.margin = unit(c(0,0,0,0), "cm")) +
  theme(legend.position = "None")

ggsave(test_plot,filename=paste0("example_095",".png"),
       device = "png",
       bg = "white",
       path = "example_plots",
       units = "px",
       width = 1200,
       height = 1200,)