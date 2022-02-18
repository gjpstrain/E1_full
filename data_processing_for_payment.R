library(tidyverse)

raw <- read_csv("pav_data.csv")

just_columns <- raw %>%
  select(c("participant", "unique_item_no", "slider.response")) %>%
  filter(unique_item_no %in% c(181,182,183,184,185,186))

my_id <- unique(just_columns$participant)

# Ignore this

x = 6

z = 6

A <- rep(1:x, each = z)

B <- rep(runif(z, min = 0.2, max = 0.9), x)

#Up till here

df <- tibble(A = rep(A, each = 6),  B = rep(B, each = 6))

dflist <- split(just_columns, just_columns$participant)

