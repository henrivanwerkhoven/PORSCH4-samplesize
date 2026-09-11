
# power for sample sizes using LOOR
table_sample_size_swcrt_scenarios <- scenarios %>%
  mutate(enrolment = sprintf("%.0f%%", enrolment*100)) %>%
  mutate(
    `Power_longitudinal ordinal` = invert_power(NiRCT, get_V_proxy(NiRCT, power.longitudinal, 0.025), 0.025), 
    `Power_days alive out of hospital` = invert_power(NiRCT, get_V_proxy(NiRCT, power.days_alive, 0.025), 0.025),
    .by=c(LOOR, proportional)) %>%
  select(LOOR, proportional, years, enrolment, N, DE, starts_with('Power_')) %>%
  arrange(LOOR, N)

table_sample_size_swcrt_scenarios
