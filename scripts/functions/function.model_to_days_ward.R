model_to_days_ward <- function(...){
  model_probabilities(...) %>%
    filter(y=='Non-ICU ward') %>%
    pull(prob) %>%
    sum()
}