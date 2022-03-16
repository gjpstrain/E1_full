library(tidyverse)
library(afex)
library(emmeans)
library(lme4)
library(buildmer)

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
           "my_rs", "slider.response", "age_textbox.text"))

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

write_csv(data_to_analyse, "data_to_analyse.csv")

# build ANOVA

model <- aov_4(difference ~ size * present + (1 + size * present | participant), data = data_to_analyse)
summary(model)

emmeans(model, pairwise ~ size * present)               

# run analysis only on those who have passed

passed <- all_passed %>%
  filter(passed == TRUE)

only_those_passed <- inner_join(data_to_analyse, passed, by = "participant")

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
  summarise(mean = mean(difference))

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

# Modelling with **buildmer**

contrasts(only_those_passed$size) = contr.sum(levels(only_those_passed$size))
contrasts(only_those_passed$present) = contr.sum(levels(only_those_passed$present))

model <- buildmer(difference ~ size * present + 
                    (1 + size * present | participant) + 
                    (1 + size * present | item),
                  data = only_those_passed)

emmeans(model@model, pairwise ~ size)
emmeans(model@model, pairwise ~ present)

summary(model)

mod2 <- lmer(difference ~ size * present +
       (1 | participant) +
       (1 | item),
     data = only_those_passed)


