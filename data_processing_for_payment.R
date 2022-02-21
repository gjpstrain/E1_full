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
    unique_item_no == 181 ~ 0.1,
    unique_item_no == 182 ~ 0.1,
    unique_item_no == 183 ~ 0.1,
    unique_item_no == 184 ~ 0.9,
    unique_item_no == 185 ~ 0.9,
    unique_item_no == 186 ~ 0.9
  )) %>%
  mutate(correct = case_when(
    unique_item_no < 184 ~ slider.response < answer,
    unique_item_no > 183 ~ slider.response > answer
  )) %>%
  group_by(participant) %>%
  summarise(total_correct = sum(correct)) %>%
  arrange(-total_correct)

new_df$passed <- new_df$total_correct > 3

write_csv(new_df, "passed.csv")

# The data frame new_df has two columns - participant id, and total attention check questions correct

f1 <- subset(new_df, total_correct == 5)

f2 <- inner_join(just_columns, f1, by = "participant")

f3 <- subset(f2, slider.response > 0.2)

f4 <- subset(f3, slider.response < 0.9)
