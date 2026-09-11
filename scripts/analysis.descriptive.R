plot_state_occupancy_ab_drainage_crude <- plot_state_occupancy(data_outcome_ab_drainage, type='crude')
plot_state_occupancy_ab_drainage_crude

# proportion died after 90 / 180 days
mort_crude_ab_drainage <- data_outcome_ab_drainage %>%
  filter(day == 0 | yprev != 'Dead') %>%
  summarise(
    N.patients = sum(day == 0),
    N.died = sum(y == 'Dead')) %>%
  mutate(
    Prop.died = N.died / N.patients)
mort_crude_ab_drainage

# note that N.icu.new is the number of new admissions. A patient may have more than one admission.
icu_crude_ab_drainage <- data_outcome_ab_drainage %>%
  summarise(
    N.patients = sum(day==0),
    N.icu.new = sum(y == 'ICU' & yprev != 'ICU' & !is.na(yprev)),
    icu.days = sum(y == 'ICU')) %>%
  mutate(
    Mean.icu.days.overall = icu.days / N.patients)
icu_crude_ab_drainage

readmission_crude_ab_drainage <- data_outcome_ab_drainage %>%
  summarise(
    N.patients = sum(day==0),
    N.readmissions = sum(y %in% c('Non-ICU ward','ICU') & yprev == 'Discharged' & !is.na(yprev))) %>%
  mutate(
    Mean.readmissions = N.readmissions / N.patients)
readmission_crude_ab_drainage
