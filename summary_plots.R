library(tidyverse)
library(hablar)
library(ggpubr)
library(scales)

theme_set(theme_pubr())

# You need to run the first chunk from E1_report.rmd before using this script

# Create mean error curve plot

curve_plot <- only_those_passed %>%
  group_by(item) %>%
  summarise(mean_obj = mean(my_rs),
            mean_subj = mean(slider.response), sd_subj = sd(slider.response)) %>%
  ggplot(aes(x = mean_obj, y = mean_subj)) +
  geom_point() +
  stat_smooth(method = "loess",
              se = FALSE) +
  geom_errorbar(aes(ymin = mean_subj-sd_subj, ymax = mean_subj+sd_subj), width = .01, colour = "forestgreen") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,1) +
  ylim(0,1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Objective Correlation Rating",
       y = "Subjective Correlation",
       title = "Mean Correlation Bias Over All Conditions")

# Create df for plotting barplot

df <- all_data %>%
  filter(!is.na(my_rs)) %>%
  dplyr::select("plots_with_labels", "my_rs",
                "slider.response") %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = ".png", replacement = "")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "all_plots/", replacement = "")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "S", replacement = "S-")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "M", replacement = "M-")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "L", replacement = "L-")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "N", replacement = "Off")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "Y", replacement = "On")) %>%
  separate(plots_with_labels, c("size", "present"), sep = "-") %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(my_rs != 0) %>%
  filter(my_rs != 1) %>%
  convert(fct("size", "present"),
          dbl("difference"))

# Use gsub to remove item numbers (not relevant)  

df$size <- gsub('[0-9.]', '', df$size)

# Create bar chart

bar_chart <- df %>%
  ggplot(aes(x = reorder(size, -difference),
             y = difference, fill = present)) +
  geom_bar(position = position_dodge2(reverse = TRUE, padding = 0),
           stat = "summary", fun.y = "mean", color = "black") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = .3,
               position = position_dodge(-.9)) +
 theme_minimal() +
  labs(x = "Condition",
       y = "Mean Bias",
       title = "Comparing Mean Difference Between Ratings and True r Values",
       fill = "Contrast\nEncoding",
       subtitle = "Standard Error Bars Shown") +
  scale_x_discrete("", labels = c("Large", "Medium", "Small"))


ggsave("bar_chart_MVN.png", bar_chart, height = 6, width = 10, dpi = 600, bg = "white")

# Use the following for larger text; suitable for posters

theme(axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 19, face = "bold"),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 18))














