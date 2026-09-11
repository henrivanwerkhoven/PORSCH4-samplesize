# calculate for each scenario what it implies for different components of the LOOM
scenarios <- scenarios  %>%
  left_join(
    scenarios %>%
      distinct(LOOR_ctr) %>%
      rowwise() %>%
      mutate(
        mort_ctr = model_to_mort(fit_ab_drainage, futime=futime, OR=LOOR_ctr),
        daooh_ctr = model_to_days_home(fit_ab_drainage, futime=futime, OR=LOOR_ctr),
        icudays_ctr = model_to_days_icu(fit_ab_drainage, futime=futime, OR=LOOR_ctr)) %>%
      ungroup(),
    by="LOOR_ctr") %>%
  left_join(
    scenarios %>%
      distinct(LOOR_int, LOOR_int_mort) %>%
      rowwise() %>%
      mutate(
        mort_int = model_to_mort(fit_ab_drainage, futime=futime, OR=LOOR_int, OR.mort=LOOR_int_mort),
        daooh_int = model_to_days_home(fit_ab_drainage, futime=futime, OR=LOOR_int, OR.mort=LOOR_int_mort),
        icudays_int = model_to_days_icu(fit_ab_drainage, futime=futime, OR=LOOR_int, OR.mort=LOOR_int_mort)) %>%
      ungroup(),
    by=c('LOOR_int','LOOR_int_mort')) %>%
  relocate(mort_ctr, mort_int, daooh_ctr, daooh_int, icudays_ctr, icudays_int, .before = years)


# rowwise() %>%
#   mutate(
#     mort_ctr = model_to_mort(fit_ab_drainage, futime=futime, OR=LOOR_ctr),
#     mort_int = model_to_mort(fit_ab_drainage, futime=futime, OR=LOOR_int, OR.mort=LOOR_int_mort),
#     daooh_ctr = model_to_days_home(fit_ab_drainage, futime=futime, OR=LOOR_ctr),
#     daooh_int = model_to_days_home(fit_ab_drainage, futime=futime, OR=LOOR_int, OR.mort=LOOR_int_mort),
#     icudays_ctr = model_to_days_icu(fit_ab_drainage, futime=futime, OR=LOOR_ctr),
#     icudays_int = model_to_days_icu(fit_ab_drainage, futime=futime, OR=LOOR_int, OR.mort=LOOR_int_mort),
#     .before = years) %>%
#   ungroup()
