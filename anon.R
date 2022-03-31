library(tidyverse)

getwd()

demo <- rbind(read_csv("data/demographics_a.csv"), read_csv("data/demographics_b.csv"))

write_csv(demo, "data/full_demographics.csv")

data <- read_csv("data/pav_data_all.csv")

tidyD <- read_csv("data/tidy_demographics.csv") %>%
  rename(participant = participant_id)

x <- left_join(data, tidyD, by = "participant")

colnames(x)
