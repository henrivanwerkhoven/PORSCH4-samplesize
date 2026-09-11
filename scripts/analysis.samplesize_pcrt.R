
# CV
CV_ab_drainage <- data_outcome_ab_drainage %>% 
  distinct(id, site) %>% 
  count(site) %>% 
  pull(n) %>%
  {sd(.) / mean(.)}

CV_drainage <- data_outcome_drainage %>% 
  distinct(id, site) %>% 
  count(site) %>% 
  pull(n) %>%
  {sd(.) / mean(.)}

# sample sizes for 90% power derived from the simulations and extrapolated to stepped-wedge trial
table_sample_size_pcrt_scenarios <- scenarios %>%
  mutate(power = power, alpha=alpha) %>%
  summarise(
    V_proxy = get_V_proxy(n.group, power.longitudinal, first(alpha)),
    n_iRCT = ceiling(invert_N(V_proxy, first(power), first(alpha))),
    .by=c(population, OR.mort, power, alpha)) %>%
  select(-V_proxy) %>%
  expand_grid(icc=icc, k=n_clusters) %>%
  rowwise() %>%
  mutate(
    PCRT = pcr_sample_size(n_iRCT, icc = icc, k=k) %>%
      select(-k, -icc) %>%
      list()) %>%
  ungroup() %>%
  unnest(PCRT) %>%
  mutate(total_size = ceiling(total_size))

table_sample_size_pcrt_scenarios
