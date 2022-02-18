library(tidyverse)

raw <- read_csv("pav_data.csv")

just_columns <- raw %>%
  select(c("participant", "unique_item_no", "slider.response")) %>%
  filter(unique_item_no %in% c(181,182,183,184,185,186))

my_id <- unique(just_columns$participant)

just_columns$answer = NULL

if (just_columns$unique_item_no == 184){
  just_columns$answer = 1
}

new_df <- just_columns %>%
  mutate(answer = case_when(
    unique_item_no == 181 ~ 0.05,
    unique_item_no == 182 ~ 0.05,
    unique_item_no == 183 ~ 0.05,
    unique_item_no == 184 ~ 0.05,
    unique_item_no == 185 ~ 0.95,
    unique_item_no == 186 ~ 0.95,
    unique_item_no == 187 ~ 0.95,
    unique_item_no == 188 ~ 0.95
  )) %>%
  mutate(correct = case_when(
    unique_item_no < 185 ~ slider.response < answer,
    unique_item_no > 184 ~ slider.response > answer
  )) %>%
  group_by(participant) %>%
  summarise(total_correct = sum(correct)) %>%
  arrange(-total_correct)

# The data frame new_df has two columns - participant id, and total attention check questions correct

# Ignore this

x = 6

z = 6

A <- rep(1:x, each = z)

B <- rep(runif(z, min = 0.2, max = 0.9), x)

#Up till here

df <- tibble(A = rep(A, each = 6),  B = rep(B, each = 6))

dflist <- split(just_columns, just_columns$participant)

