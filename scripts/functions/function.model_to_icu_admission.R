model_to_icu_admission <- function(...){
  model_probabilities(..., keep.yprev = TRUE) %>%
    filter(y %in% c('ICU'), yprev!='ICU') %>%
    pull(prob) %>%
    sum()
}