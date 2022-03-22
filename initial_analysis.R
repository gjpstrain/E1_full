library(tidyverse)
library(afex)
library(emmeans)
library(lme4)
library(buildmer)
library(gridExtra)
library(wesanderson)

setwd("R_work/E1_data_processing")

all_data <- rbind(read_csv("pav_data_a.csv"), read_csv("pav_data_b.csv"))

all_passed <- rbind(read_csv("passed_a.csv"), read_csv("passed_b.csv"))

# separate the plots_with_labels column into 3 - item, size (3 levels), and present (2 levels)

separated_data <- 
  all_data %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "S", replacement = "-S-")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "M", replacement = "-M-")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "L", replacement = "-L-")) %>%
  separate(plots_with_labels, c("item", "size", "present"), sep = "-") %>%
  mutate(present = str_replace(present, pattern = ".png", replacement = "")) %>%
  mutate(item = str_replace(item, pattern = "all_plots/", replacement = "")) %>%
  select(c("item", "size", "present", "participant",
           "q1_slider.response", "q2_slider.response",
           "q3_slider.response", "q4_slider.response",
           "q5_slider.response", "res_vec", "unique_item_no",
           "my_rs", "slider.response", "age_textbox.text")) %>%
  filter(unique_item_no < 181)


# plot data

separated_data  %>%
  mutate(size = as.factor(size)) %>%
  mutate(present = as.factor(present)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size)) %>%
  filter(!is.na(difference)) %>%
  ggplot(aes(x = size:present, y = difference)) +
  geom_violin() +
  stat_summary(fun.data = mean_cl_boot)

# calculate descriptives

separated_data  %>%
  mutate(size = as.factor(size)) %>%
  mutate(present = as.factor(present)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size)) %>%
  filter(!is.na(difference)) %>%
  group_by(size, present) %>%
  summarise(mean = mean(difference))

# get data ready for analysis

data_to_analyse <- separated_data  %>%
  mutate(size = as.factor(size)) %>%
  mutate(present = as.factor(present)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(difference)) %>%
  select(-c(q1_slider.response:q5_slider.response))



# build ANOVA

model <- aov_4(difference ~ size * present + (1 + size * present | participant), data = only_those_passed)
summary(model)

emmeans(model, pairwise ~ size * present)               

# run analysis only on those who have passed

passed <- all_passed %>%
  filter(passed == TRUE)

only_those_passed <- inner_join(data_to_analyse, passed, by = "participant")

write_csv(only_those_passed, "data_to_analyse.csv")

# calculate descriptives

only_those_passed  %>%
  mutate(size = as.factor(size)) %>%
  mutate(present = as.factor(present)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size)) %>%
  filter(!is.na(difference)) %>%
  group_by(size, present) %>%
  summarise(mean = mean(difference))

only_those_passed   %>%
  mutate(size = as.factor(size)) %>%
  mutate(present = as.factor(present)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size)) %>%
  filter(!is.na(difference)) %>%
  group_by(present) %>%
  summarise(mean = mean(difference), sd = sd(difference))

only_those_passed  %>%
  mutate(size = as.factor(size)) %>%
  mutate(present = as.factor(present)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size)) %>%
  filter(!is.na(difference)) %>%
  group_by(size) %>%
  summarise(mean = mean(difference))

model_passed <- aov_4(difference ~ size * present + (1 + size * present | participant), data = only_those_passed)
summary(model_passed)

emmeans(model_passed, pairwise ~ present)

# Manually Modelling

manual_model <- lmer(difference ~ size * present +
       (1 | participant) +
       (1 | item),
     data = only_those_passed)

emmeans(manual_model, pairwise ~ size)
emmeans(manual_model, pairwise ~ present)

# Modelling with **buildmer**

contrasts(only_those_passed$size) = contr.sum(levels(only_those_passed$size))
contrasts(only_those_passed$present) = contr.sum(levels(only_those_passed$present))

model <- buildmer(difference ~ size * present + 
                    (1 + size * present | participant) + 
                    (1 + size * present | item),
                  data = only_those_passed)

emmeans(model@model, pairwise ~ size)
emmeans(model@model, pairwise ~ present)


# Visualisation


present <- only_those_passed %>%
  filter(present == "Y") %>%
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
  labs(x = "Objective Correlation Rating",
       y = "Subjective Correlation",
       title = "Encoding Present")

absent <- only_those_passed %>%
  filter(present == "N") %>%
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
  theme(plot.title = element_text(hjust =0.9)) +
  labs(x = "Objective Correlation Rating",
       y = "Subjective Correlation",
       title = "Encoding Absent")   

final_plot <- grid.arrange(present, absent,ncol = 2, top = "Comparing Correlation Ratings")  

ggsave("encoding_effect.png",plot = final_plot, width = 6000, height = 3000, units = "px", dpi = 600)

# Modelling with graph literacy


  
  
  
  
  