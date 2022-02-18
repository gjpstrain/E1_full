library(tidyverse)
library(afex)
library(emmeans)
library(lme4)

my_data <- read_csv("pav_data.csv")

# separate the plots_with_labels column into 3 - item, size (3 levels), and present (2 levels)

separated_data <- 
  my_data %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "S", replacement = "-S-")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "M", replacement = "-M-")) %>%
  mutate(plots_with_labels = str_replace(plots_with_labels, pattern = "L", replacement = "-L-")) %>%
  separate(plots_with_labels, c("item", "size", "present"), sep = "-") 

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
  filter(!is.na(size_w)) %>%
  filter(!is.na(difference)) 

# build ANOVA

model <- aov_4(difference ~ size * present + (1 + size * present | participant), data = data_to_analyse)
summary(model)

emmeans(model, pairwise ~ size * present)               

# run analysis only on those who have passed

passed <- read_csv("passed.csv") %>%
  filter(passed == TRUE)

only_those_passed <- inner_join(data_to_analyse, passed, by = "participant")

# calculate descriptives

only_those_passed   %>%
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

emmeans(model_passed, pairwise ~ size * present)    

# mixed model

model_mixed <- lmer(difference ~ size * present + 
                      (1 + size * present | participant) + 
                      (1 + size * present | item), data = only_those_passed)

summary(model_mixed)

emmeans(model_mixed, pairwise ~ size * present)    
