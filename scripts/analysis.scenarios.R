# icc based on data is 0.016 (95% CI: 0.008-0.056; 200 bootstraps)

scenarios <- expand_grid(
  k = 12,
  icc = 0.016,
  tibble(LOOR = c(.800, .825, 0.850)) %>%
    mutate(
      LOOR_ctr = LOOR.control,
      LOOR_int = LOOR.control * LOOR),
  tribble(
    ~years, ~enrolment, ~N,
    2,   1,  948,
    2.5, 1,  1195,
    2,   .8, 758,
    2.5, .8, 956))

# calculate back from a stepped-wedge trial what is the design effect (hence the effective sample size)

scenarios <- scenarios %>%
  rowwise() %>%
  mutate(DE = swcr_design_effect(N/k/(k+1), icc, k)) %>%
  ungroup() %>%
  # now we can calculate the number of patients per group needed for the simulations
  # so that we can run simulations as if this were an individually randomized trial
  mutate(NiRCT = N / DE / 2)

# for each scenario, find an odds ratio for the transition from ICU to death that calibrates the intervention effect to have a 1% absolute risk rediction for mortality
# create additional scenarios for this
p_mort <- scenarios %>% 
  distinct(LOOR_ctr) %>%
  rowwise() %>%
  mutate(p_mort_ctr = model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR_ctr)) %>%
  ungroup()

scenarios <- left_join(scenarios, p_mort, by='LOOR_ctr')

p_mort <- scenarios %>%
  distinct(LOOR_int) %>%
  rowwise() %>%
  mutate(p_mort_int = model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR_int)) %>%
  ungroup()

scenarios <- left_join(scenarios, p_mort, by='LOOR_int')

LOOR_int_mort <- scenarios %>%
  distinct(LOOR_int, p_mort_ctr) %>%
  rowwise() %>%
  mutate(LOOR_int_mort = {
    optimize(function(OR_int_mort){
      p_mort_int = model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR_int, OR.mort=OR_int_mort)
      abs(p_mort_int - p_mort_ctr + 0.01) # aiming for a -1% ARD
    }, interval = c(LOOR_int, 10))$minimum
    }) %>%
  ungroup()

scenarios <- bind_rows(
  scenarios %>% 
    mutate(LOOR_int_mort = LOOR_int, proportional = TRUE, .after = LOOR_int),
  scenarios %>%
    left_join(LOOR_int_mort, by=c('LOOR_int','p_mort_ctr')) %>%
    mutate(proportional = FALSE) %>%
    relocate(LOOR_int_mort, proportional, .after=LOOR_int)) %>%
  select(-p_mort_ctr)

# cleaning
rm(p_mort, LOOR_int_mort)
