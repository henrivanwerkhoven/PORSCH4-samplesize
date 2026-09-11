# fit model population 1: ab_drainage

dtmar_ab_drainage <- data_outcome_ab_drainage %>%
  # remove day 0 for modeling
  filter(day > 0) %>%
  # make sure if patient dies all remaining records are removed
  filter(
    yprev != 'Dead') %>%
  # set yprev as non-ordered factor for modeling
  mutate(
    yprev = factor(yprev, ordered=FALSE)) 

fit_ab_drainage <- vglm(
  y ~ yprev + bs(day, df=5),
  cumulative(reverse=TRUE, parallel=FALSE ~  bs(day, df=5)), 
  data=dtmar_ab_drainage)
