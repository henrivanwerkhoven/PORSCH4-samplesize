# load data on ordinal outcome

data_outcome_ab_drainage <- "data/data_outcome_aggregated.csv" %>%
  read.csv2() %>%
  mutate(
    across(
      c(y, yprev), 
      ~ factor(.x, c('Discharged', 'Non-ICU ward', 'ICU', 'Dead'), ordered = TRUE))) %>%
  uncount(n)

