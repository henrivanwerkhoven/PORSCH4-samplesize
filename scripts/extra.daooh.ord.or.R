# get the ordinal OR for the days alive out of hospital endpoint
futime <- 90
simdat <- simulate_trial(
  model = fit_ab_drainage, 
  n.group = 10000, 
  ystats = data_outcome_ab_drainage$y %>% levels(), 
  futime = futime-1, 
  OR.intervention = .80)

# alternative outcome measure days alive out of the hospital
days_alive <- simdat %>%
  filter(y != 'Dead') %>%
  summarise(
    days_alive_out_of_hospital = sum(y == 'Discharged'),
    .by=c(id,tx)) %>%
  mutate(
    y = days_alive_out_of_hospital %>%
      ordered(0:futime))

fit.days <- polr(y ~ tx, data=days_alive, Hess=TRUE)
exp(coef(fit.days)['tx'])
