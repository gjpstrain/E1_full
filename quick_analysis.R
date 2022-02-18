library(tidyverse)
library(afex)
library(emmeans)

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
