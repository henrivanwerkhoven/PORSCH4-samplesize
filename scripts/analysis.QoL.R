PCS_summary <- data_sf36 %>% 
  full_join(
    data_outcome %>% 
      summarise(alive = !any(outcome_score==4), .by=id),
    by='id', relationship='one-to-one') %>%
  summarise(
    N.alive = sum(alive),
    p.alive = mean(alive),
    N.SF36 = sum(!is.na(PCS)),
    p.SF36 = mean(!is.na(PCS[alive])),
    pct = mean(!is.na(PCS)),
    mean = mean(PCS, na.rm=TRUE),
    sd = sd(PCS, na.rm=TRUE))

MCS_summary <- data_sf36 %>% 
  summarise(
    N.SF36 = sum(!is.na(MCS)),
    mean = mean(MCS, na.rm=TRUE),
    sd = sd(MCS, na.rm=TRUE))

# tmp dataset of those surviving 90 days
x <- data_sf36 %>% 
  filter(!is.na(PCS)) %>%
  left_join(data_outcome_summary, by='id', relationship='one-to-one')

# scatter plot 
plot_mean_outcome_vs_SF36_d90 <- x %>%
  pivot_longer(cols=c(PCS,MCS), names_to='component', values_to='score') %>%
  mutate(component = component %>% 
           factor(c('PCS','MCS'), c('Physical Component Summary', 'Mental Component Summary'))) %>%
  ggplot(aes(x=mean_outcome_score, y=score)) +
  geom_point() +
  geom_smooth(method = 'gam', formula = y ~ s(x, bs="cs")) +
  facet_grid(cols=vars(component)) +
  labs(x="Mean ordinal outcome", y="SF-36 Component Summary score")

plot_mean_outcome_vs_SF36_d90

# correlation of mean ordinal outcome with PCS in 90-day survivors
cor_spear_PCS_mean_outcome <- c(
  with(x, cor(mean_outcome_score, PCS), method="spear"),
  sapply(1:2000, function(i){
    x %>% 
      slice_sample(prop=1, replace=TRUE) %>%
      with(cor(mean_outcome_score, PCS), method="spear")}) %>%
    quantile(c(.025,.975)))
cor_spear_PCS_mean_outcome

# correlation of mean ordinal outcome with MCS in 90-day survivors
cor_spear_MCS_mean_outcome <- c(
  with(x, cor(mean_outcome_score, MCS, method="spearman")),
  sapply(1:2000, function(i){
    x %>% 
      slice_sample(prop=1, replace=TRUE) %>%
      with(cor(mean_outcome_score, MCS, method="spear"))}) %>%
    quantile(c(.025,.975)))
cor_spear_MCS_mean_outcome

rm(x)