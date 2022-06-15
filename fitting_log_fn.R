absent2 <- only_those_passed %>%
  filter(present == "N") %>%
  group_by(item) %>%
  summarise(mean_obj = mean(my_rs),
            mean_subj = mean(slider.response), sd_subj = sd(slider.response))
  
absent3 <- absent2 %>%
  add_row(tibble_row(item = "32", mean_obj = 0, mean_subj = 0)) %>%
  add_row(tibble_row(item = "33", mean_obj = 1, mean_subj = 1)) %>%
  mutate(lg6 = (log(1-0.6*mean_obj)/log(1-0.6))) %>%
  mutate(lg65 = (log(1-0.65*mean_obj)/log(1-0.65))) %>%
  mutate(lg7 = (log(1-0.7*mean_obj)/log(1-0.7))) %>%
  mutate(lg8 = (log(1-0.8*mean_obj)/log(1-0.8))) %>%
  mutate(lg85 = (log(1-0.85*mean_obj)/log(1-0.85)))

p <- ggplot(data = absent2, aes(x = mean_obj, y = mean_subj)) +
  geom_point() +
  stat_smooth(method = "loess",
              se = FALSE) +
 # geom_errorbar(aes(ymin = mean_subj-sd_subj, ymax = mean_subj+sd_subj), width = .01, colour = "forestgreen") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,1) +
  ylim(0,1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust =0.9)) +
  labs(x = "Objective Correlation Rating",
       y = "Subjective Correlation",
       title = "Encoding Absent")


ggplot(data = absent3, aes(x = mean_obj, y = mean_subj)) +
  geom_smooth(aes(x = mean_obj, y = mean_subj, colour = "Observed")) +
  geom_smooth(aes(x = mean_obj, y = lg6, colour = "lg6")) +
  geom_smooth(aes(x = mean_obj, y = lg65, colour = "lg65")) +
  geom_smooth(aes(x = mean_obj, y = lg7, colour = "lg7")) +
  geom_smooth(aes(x = mean_obj, y = lg85, colour = "lg8")) +
  geom_abline(intercept = 0, slope = 1) +
  xlim(0,1) +
  ylim(0,1) +
  theme_minimal()
