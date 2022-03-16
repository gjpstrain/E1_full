library(tidyverse)
library(MASS)

# Change the variable below according to how many trials you want ####

number_of_participants = 1

number_of_trials = 400

set.seed(1234)

my_ss <- rep(1:number_of_participants, each = number_of_trials)

my_rs <- rep(runif(number_of_trials, min = 0.2, max = 0.9), number_of_participants)

correlations <- tibble(my_ss, my_rs)


                     