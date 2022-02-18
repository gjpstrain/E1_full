library(tidyverse)
library(afex)
library(emmeans)

my_data <- read_csv("pav_data.csv")

my_data %>% 
  filter(my_rs !=0 & my_rs != 1) %>%
  ggplot(aes(x = slider.response, y = my_rs)) +
  geom_point()

my_data %>%
  mutate(size_w = as.factor(size_w)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size_w)) %>%
  filter(!is.na(difference)) %>%
  ggplot(aes(x = size_w, y = difference)) +
  geom_violin() +
  stat_summary(fun.data = mean_cl_boot)

my_data %>%
  mutate(size_w = as.factor(size_w)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size_w)) %>%
  filter(!is.na(difference)) %>%
  group_by(size_w) %>%
  summarise(mean = mean(difference))

data_to_analyse <- my_data %>%
  mutate(size_w = as.factor(size_w)) %>%
  mutate(difference = my_rs - slider.response) %>%
  filter(!is.na(size_w)) %>%
  filter(!is.na(difference)) 

model <- aov_4(difference ~ size_w + (1 + size_w | participant), data = data_to_analyse)
summary(model)

emmeans(model, pairwise ~ size_w)               
