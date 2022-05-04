library(tidyverse)
library(hablar)
library(ggpubr)

theme_set(theme_pubr())


p1 <-only_those_passed %>%
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

df <- all_data %>%
  filter(!is.na(my_rs)) %>%
  dplyr::select("plots_with_labels", "my_rs",
                "slider.response") %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = ".png", replacement = "")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "all_plots/", replacement = "")) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(my_rs != 0) %>%
  filter(my_rs != 1) %>%
  convert(fct("plots_with_labels"),
          dbl("difference"))
  

df$plots_with_labels <- gsub('[0-9.]', '', df$plots_with_labels)

p2 <- df %>%
  group_by(plots_with_labels) %>%
  ggplot(aes(x = reorder(plots_with_labels, difference, na.rm = TRUE),
             y = difference, color = plots_with_labels )) +
  geom_boxplot() +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete("", labels = c("Large\nEncoding Present",
                                        "Medium\nEncoding Present",
                                        "Small\nEncoding Present",
                                        "Large\nEncoding Absent",
                                        "Medium\nEncoding Absent",
                                        "Small\nEncoding Absent")) +
  labs(x = "Condition",
       y = "Difference",
       title = "Comparing Mean Difference Between Ratings and True r Values")

combined_figure <- ggarrange(p1,p2, ncol = 1)

ggsave("figure_MVN.png", combined_figure, height = 15, width = 7.5, dpi = 800)















