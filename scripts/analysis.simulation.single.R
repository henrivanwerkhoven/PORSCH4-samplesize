## for population 1: ab_drainage

# plot simulated outcome distribution for a single large simulated trial
ystats <- dtmar_ab_drainage$y %>% levels()

OR.control <- model_output_ab_drainage$ORs$`0.5`$control
OR.intervention <- model_output_ab_drainage$ORs$`0.5`$intervention

simdat <- simulate_trial(model = fit_ab_drainage, n.group = 5000, ystats = ystats, futime = futime-1,
                         OR.control = OR.control, OR.intervention = OR.intervention)

# # previously I used sim.OR and let placebo group have OR of 1
# # to get this behaviour, set OR.intervention = sim.OR and OR.placebo = 1
# simdat <- simulate_trial(h1, 2500, ystats, sim.OR)

extra_days_dead <- simdat %>%
  filter(y == ystats[4]) %>%
  uncount(futime-1-day) %>%
  mutate(day = (day[1]+1):(futime-1), .by=id)

plot_state_occupancy_ab_drainage_simulated <- plot_state_occupancy(bind_rows(simdat,extra_days_dead), type='simulated')
plot_state_occupancy_ab_drainage_simulated

# visualize days alive out of the hospital endpoint
plot_days_alive_out_of_hospital_ab_drainage_simulated <- simdat %>%
  summarise(
    days_alive_home = if_else(
      any(y == 'Dead'),
      -1,
      sum(y=='Discharged')
    ),
    .by=c(id,tx)
  ) %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  ggplot(aes(x=days_alive_home))+
  geom_histogram(, bins=92) +
  facet_wrap(vars(tx), nrow=2)
plot_days_alive_out_of_hospital_ab_drainage_simulated

# mortality figure derived by the simulation with large sample size
mort_sim_ab_drainage <- simdat %>%
  arrange(id) %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  summarise(died = any(y == 'Dead'), .by=c(id, tx)) %>%
  summarise(
    n = n(),
    `death by day 90` = sum(died),
    pct = mean(died)*100, 
    .by=tx) %>%
  mutate(
    RD = if_else(tx=='Intervention', diff(pct) %>% sprintf(fmt="%.1f%%"), '-'),
    pct = sprintf("%.1f%%", pct))
mort_sim_ab_drainage 

# ICU admissions derived by the simulation with large sample size
icu_sim_ab_drainage <- simdat %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  summarise(
    icu.any = any(y == 'ICU'), 
    icu.days = sum(y == 'ICU'),
    .by=c(id,tx)) %>%
  summarise(
    n = n(),
    N.icu.anytime = sum(icu.any),
    pct.icu.anytime = sprintf(fmt="%.1f%%", mean(icu.any)*100),
    mean.icu.days = mean(icu.days[icu.days > 0]),
    .by=tx) 
icu_sim_ab_drainage

# Readmissions derived by the simulation with large sample size
readmission_sim_ab_drainage <- simdat %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  summarise(
    readmission_num = sum(y %in% c('Non-ICU ward','ICU') & lag(y, default = 'Non-ICU ward') == 'Discharged'),
    .by=c(id,tx)) %>%
  summarise(
    N.patients = n(),
    N.readmissions = sum(readmission_num),
    Mean.readmissions = mean(readmission_num),
    .by=tx)
readmission_sim_ab_drainage


## for population 2: drainage

# plot simulated outcome distribution for a single large simulated trial
ystats <- dtmar_drainage$y %>% levels()

OR.control <- model_output_drainage$ORs$`0.5`$control
OR.intervention <- model_output_drainage$ORs$`0.5`$intervention

simdat <- simulate_trial(model = fit_drainage, n.group = 5000, ystats = ystats, futime = futime-1,
                         OR.control = OR.control, OR.intervention = OR.intervention)

# # previously I used sim.OR and let placebo group have OR of 1
# # to get this behaviour, set OR.intervention = sim.OR and OR.placebo = 1
# simdat <- simulate_trial(h1, 2500, ystats, sim.OR)

extra_days_dead <- simdat %>%
  filter(y == ystats[4]) %>%
  uncount(futime-1-day) %>%
  mutate(day = (day[1]+1):(futime-1), .by=id)

plot_state_occupancy_drainage_simulated <- plot_state_occupancy(bind_rows(simdat,extra_days_dead), type='simulated')
plot_state_occupancy_drainage_simulated

# visualize days alive out of the hospital endpoint
plot_days_alive_out_of_hospital_drainage_simulated <- simdat %>%
  summarise(
    days_alive_home = if_else(
      any(y == 'Dead'),
      -1,
      sum(y=='Discharged')
    ),
    .by=c(id,tx)
  ) %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  ggplot(aes(x=days_alive_home))+
  geom_histogram(, bins=92) +
  facet_wrap(vars(tx), nrow=2)
plot_days_alive_out_of_hospital_drainage_simulated

# mortality figure derived by the simulation with large sample size
mort_sim_drainage <- simdat %>%
  arrange(id) %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  summarise(died = any(y == 'Dead'), .by=c(id, tx)) %>%
  summarise(
    n = n(),
    `death by day 90` = sum(died),
    pct = mean(died)*100, 
    .by=tx) %>%
  mutate(
    RD = if_else(tx=='Intervention', diff(pct) %>% sprintf(fmt="%.1f%%"), '-'),
    pct = sprintf("%.1f%%", pct))
mort_sim_drainage 

# ICU admissions derived by the simulation with large sample size
icu_sim_drainage <- simdat %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  summarise(
    icu.any = any(y == 'ICU'), 
    icu.days = sum(y == 'ICU'),
    .by=c(id,tx)) %>%
  summarise(
    n = n(),
    N.icu.anytime = sum(icu.any),
    pct.icu.anytime = sprintf(fmt="%.1f%%", mean(icu.any)*100),
    mean.icu.days = mean(icu.days[icu.days > 0]),
    .by=tx) 
icu_sim_drainage

# Readmissions derived by the simulation with large sample size
readmission_sim_drainage <- simdat %>%
  mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
  summarise(
    readmission_num = sum(y %in% c('Non-ICU ward','ICU') & lag(y, default = 'Non-ICU ward') == 'Discharged'),
    .by=c(id,tx)) %>%
  summarise(
    N.patients = n(),
    N.readmissions = sum(readmission_num),
    Mean.readmissions = mean(readmission_num),
    .by=tx)
readmission_sim_drainage
