plot_state_occupancy_ab_drainage_simulated <- scenarios %>%
  pivot_longer(cols = c(LOOR_ctr, LOOR_int), names_to='group', values_to='LOOR_sim') %>%
  mutate(LOOR = if_else(group == 'LOOR_ctr', 1, LOOR)) %>%
  mutate(LOOR_int_mort = if_else(LOOR==1, NA_real_, LOOR_int_mort)) %>%
  distinct(LOOR, LOOR_sim, LOOR_int_mort, proportional) %>%
  filter(LOOR != 1 | proportional) %>%
  arrange(desc(LOOR), desc(proportional)) %>%
  rowwise() %>%
  mutate(plotx = (
    plot_state_occupancy(data = dtmar_ab_drainage, 
                         model = fit_ab_drainage, 
                         type = 'model',
                         OR = LOOR_sim,
                         OR.mort = if(!proportional & LOOR != 1) LOOR_int_mort else LOOR_sim) +
      labs(title="Modeled state occupancy probability (LOOR = %.3f%s)" %>% 
           sprintf(LOOR, if(LOOR == 1) '' else if(proportional) '; proportional' else '; non-proportional'))) %>% 
      list()) %>%
  ungroup() %>%
  pull(plotx)

plot_state_occupancy_ab_drainage_simulated
