
# CV - I have calculated it but it seems that CV is less relevant for SW-CRT
CV_ab_drainage <- data_outcome_ab_drainage %>% 
  distinct(id, site) %>% 
  count(site) %>% 
  pull(n) %>%
  {sd(.) / mean(.)}

# CV_drainage <- data_outcome_drainage %>% 
#   distinct(id, site) %>% 
#   count(site) %>% 
#   pull(n) %>%
#   {sd(.) / mean(.)}

# # sample sizes for 90% power derived from the simulations and extrapolated to stepped-wedge trial
# table_sample_size_swcrt_scenarios <- scenarios %>%
#   mutate(power = power, alpha=alpha) %>%
#   summarise(
#     V_proxy = get_V_proxy(n.group, power.longitudinal, first(alpha)),
#     n_iRCT = ceiling(invert_N(V_proxy, first(power), first(alpha))),
#     .by=c(population, OR.mort, power, alpha)) %>%
#   select(-V_proxy) %>%
#   expand_grid(icc=icc, k=n_clusters) %>%
#   rowwise() %>%
#   mutate(
#     SWCRT = swcr_sample_size(n_iRCT, icc = icc, k=k) %>%
#       select(-k, -icc) %>%
#       list()) %>%
#   ungroup() %>%
#   unnest(SWCRT) %>%
#   mutate(total_size = ceiling(total_size))

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
