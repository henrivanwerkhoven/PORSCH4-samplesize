model_to_readmission <- function(...){
  model_probabilities(..., keep.yprev = TRUE) %>%
    filter(y %in% c('ICU','Non-ICU ward'), yprev=='Discharged') %>%
    pull(prob) %>%
    sum()
}