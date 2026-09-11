## population one: ab_drainage

# plot state occupancy probabilities predicted by the model

plot_state_occupancy_ab_drainage_model <- plot_state_occupancy(dtmar_ab_drainage, fit_ab_drainage, type='model')
plot_state_occupancy_ab_drainage_model

# crude / model predicted mortality
mort_crude <- mort_crude_ab_drainage %>% 
  pull(Prop.died)

mort_model <- model_to_mort(fit_ab_drainage, futime=futime)

# crude / model-predicted days alive out of hospital
days_alive_crude <- data_outcome_ab_drainage %>%
  summarise(
    N.patients = sum(day==0),
    days_home = sum(y == 'Discharged')) %>%
  mutate(mean_days_home = days_home / N.patients)

days_alive_model <- model_to_days_home(fit_ab_drainage, futime=futime)

# correction factor

# the model underpredicts the cumulative mortality risk
# find an odds ratio that calibrates the model to have the same 90-day mortality risk
opt <- optimize(function(OR){
  p <- model_to_mort(fit_ab_drainage, futime=futime, OR=OR)
  abs(p - mort_crude)
}, c(1e-1, 1e1))
if(opt$objective > .001) warning("The optimization may not have converged. We are assuming ORs for the control are between 0.1 and 10.")
LOOR.control <- opt$minimum
rm(opt)

model_to_mort(fit_ab_drainage, futime=futime, OR=LOOR.control)

days_alive_model_corr <- model_to_days_home(fit_ab_drainage, futime=futime, OR=LOOR.control)

admissions_icu_model_corr <- model_to_icu_admission(fit_ab_drainage, futime=futime, OR=LOOR.control)

days_icu_model_corr <- model_to_days_icu(fit_ab_drainage, futime=futime, OR=LOOR.control)

readmissions_model_corr <- model_to_readmission(fit_ab_drainage, futime=futime, OR=LOOR.control)
