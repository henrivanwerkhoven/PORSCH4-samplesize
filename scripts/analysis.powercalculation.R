
# simulated difference in 90 day mortality (median, IQR) and difference according to the simulation model

scenarios %>% 
  mutate(
    delta_mort_mod = sprintf("%.3f", mort_int - mort_ctr)) %>%
  summarise(
    delta_mort_sim = lapply(pow_sim, function(s) s$delta_mort) %>% 
      unlist() %>% 
      quantile(c(.5,.25,.75)) %>% 
      {sprintf("%.3f (IQR %.3f; %.3f)", .[1], .[2], .[3])},
    .by=c(LOOR, delta_mort_mod))

# simulated difference in days alive and out of hospital (median, IQR) and difference according to the simulation model
scenarios %>% 
  mutate(
    delta_home_mod = sprintf("%.3f", daooh_int - daooh_ctr)) %>%
  summarise(
    delta_home_sim = lapply(pow_sim, function(s) s$delta_home) %>% 
      unlist() %>% 
      quantile(c(.5,.25,.75)) %>% 
      {sprintf("%.3f (IQR %.3f; %.3f)", .[1], .[2], .[3])},
    .by=c(LOOR, delta_home_mod))


# power with longitudinal ordinal logistic regression
# power.longitudinal <- mean(pow_sim$OR.est.long['OR.upper',] < 1)
# power.longitudinal
scenarios <- scenarios %>% 
  rowwise() %>%
  mutate(power.longitudinal = mean(pow_sim$OR.est.long['OR.upper',] < 1)) %>%
  ungroup()
scenarios$power.longitudinal

# median OR with longitudinal ordinal logistic regression
scenarios %>% 
  rowwise() %>%
  mutate(LOOR_median_estimate = median(pow_sim$OR.est.long['OR',])) %>%
  ungroup() %>%
  select(LOOR, LOOR_median_estimate)

# power when analysing days alive out of hospital (Mann Whitney U identical to ordinal regression)
power.mwu <- sapply(scenarios$pow_sim, function(x) mean(x$p.wmu.days < .05))
scenarios <- scenarios %>%
  rowwise() %>%
  mutate(power.days_alive = mean(pow_sim$OR.est.days['OR.lower',] > 1, na.rm=TRUE)) %>%
  ungroup()
power.mwu
scenarios$power.days_alive

scenarios <- scenarios %>%
  rowwise() %>%
  mutate(ess_longitudinal_vs_days_alive = if(is.na(power.longitudinal) | is.na(power.days_alive)) NA else effective_sample_size(power.longitudinal, power.days_alive, alpha = .025)) %>%
  ungroup()

# power when only assessing mortality
scenarios <- scenarios %>%
  rowwise() %>%
  mutate(power.mortality = power.prop.test(p1=mort_ctr, p2=mort_int, n = NiRCT)$power) %>%
  ungroup()
scenarios$power.mortality

scenarios <- scenarios %>%
  rowwise() %>%
  mutate(ess_longitudinal_vs_mortality = effective_sample_size(power.longitudinal, power.mortality, alpha = .025)) %>%
  ungroup()

## power plot maken

plot_power_scenarios_ab_drainage <- scenarios %>%
  mutate(effect="LOOR: %.3f; %s" %>% sprintf(LOOR, if_else(proportional, 'proportional', 'non-proportional'))) %>%
  plot_power(pvars=c('power.longitudinal','power.days_alive','power.mortality'), 
             plabels=c(power.longitudinal='Longitudinal', power.days_alive='Days alive', power.mortality='Mortality'), 
             nvar='NiRCT', fvar='effect')
plot_power_scenarios_ab_drainage

table_power_scenarios <- table_power(scenarios)
table_power_scenarios
