table_power <- function(data, alpha=.025){
  data %>%
    mutate(Effect = "LOOR: %.3f; %s" %>% sprintf(LOOR, if_else(proportional, "proportinoal", "non-proportional"))) %>%
    pivot_longer(cols=starts_with('power.'), names_to='Analysis', values_to='Power simulated', names_prefix = 'power.')  %>%
    mutate(Analysis = factor(Analysis, c('mortality','days_alive','longitudinal'), c('Mortality', 'Days alive', 'Longitudinal'))) %>%
    arrange(Effect, NiRCT, Analysis) %>%
    mutate(
      `Power extrapolated` = invert_power(NiRCT, get_V_proxy(NiRCT, `Power simulated`, alpha), alpha), 
      .by=c(Effect, Analysis)) %>%
    mutate(
      `ESS vs. mortality` = sapply(`Power extrapolated`, function(x) effective_sample_size(x, `Power extrapolated`[Analysis=='Mortality'], alpha)), 
      `ESS vs. days alive` = sapply(`Power extrapolated`, function(x) effective_sample_size(x, `Power extrapolated`[Analysis=='Days alive'], alpha)), 
      .by=c(Effect, NiRCT)) %>%
    select(Effect, Analysis, `N per group`=NiRCT, starts_with('Power '), starts_with('ESS ')) %>%
    mutate(
      across(starts_with('Power '), ~ sprintf("%.1f%%", .x*100)),
      across(starts_with('ESS '), ~ if_else(.x==1, '[ref]', sprintf("%.1f", .x))))
}

