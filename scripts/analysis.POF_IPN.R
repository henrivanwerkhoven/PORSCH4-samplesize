# temporary dataset
x <- data_outcome_summary %>%
  full_join(data_POF_IPN, by='id') %>%
  mutate(
    POF_num = as.numeric(POF),
    IPN_num = as.numeric(IPN),
    POF_IPN = case_when(
      IPN=='No' & POF=='No' ~ 'None',
      IPN=='Yes' & POF=='No' ~ 'IPN only',
      IPN=='No' & POF=='Yes' ~ 'POF only',
      TRUE ~ 'POF and IPN') %>%
      # Factor labels determine order in plots / tables.
      # Note: if you change the order here, also change it in the `legend.text` attribute at the end of this script.
      factor(c('None','IPN only','POF only','POF and IPN')))

table_IPN_POF <- x %>%
  count(IPN, POF) %>%
  pivot_wider(names_from = POF, values_from = n, names_prefix = 'POF=')

table_IPN_POF

# violin plot of mean outcome score vs. occurrence of POF
plot_score_by_POF <- x %>%
  ggplot(aes(x=POF, y=mean_outcome_score)) +
  geom_violin() +
  labs(x="Persistent organ failure", y="Mean ordinal outcome score")

plot_score_by_POF

# Spearman’s Rank Correlation
cor_spear_POF_mean_outcome <- c(
  est=cor(x$POF_num, x$mean_outcome_score, method = "spearman"),
  sapply(1:200, function(i){
    x %>%
      slice_sample(prop=1, replace=TRUE) %>%
      with(cor(POF_num, mean_outcome_score, method = "spearman"))}) %>%
    quantile(c(.025,.975)))

cor_spear_POF_mean_outcome

# violin plot of mean outcome score vs. occurrence of IPN
plot_score_by_IPN <- x %>%
  ggplot(aes(x=IPN, y=mean_outcome_score)) +
  geom_violin() +
  labs(x="Infectious pancreatic necrosis", y="Mean ordinal outcome score")

plot_score_by_IPN

# Spearman’s Rank Correlation
cor_spear_IPN_mean_outcome <- c(
  est=cor(x$IPN_num, x$mean_outcome_score, method = "spearman"),
  sapply(1:200, function(i){
    x %>%
      slice_sample(prop=1, replace=TRUE) %>%
      with(cor(IPN_num, mean_outcome_score, method = "spearman"))}) %>%
    quantile(c(.025,.975)))

cor_spear_IPN_mean_outcome

# violin plot of mean outcome score vs. occurrence of IPN
plot_score_by_POF_and_IPN <- x %>%
  ggplot(aes(x=POF_IPN, y=mean_outcome_score)) +
  geom_violin() +
  labs(x="Complication status", y="Mean ordinal outcome score")

attr(plot_score_by_POF_and_IPN, 'legend.text') <- 
  'Violin plot of the mean ordinal outcome during %.0f days by presence of IPN and POFAbbreviations: IPN: infectious pancreatic necrosis, POF: persistent organ failure.' %>%
  sprintf(futime)

plot_score_by_POF_and_IPN

# state occupancy plots stratified by IPN / POF
plot_state_occupancy_by_POF_IPN <- x %>%
  full_join(data_outcome, by='id', relationship = 'one-to-many') %>%
  summarise(
    plot = plot_state_occupancy(data=pick(everything()), type='crude') %>% list(),
    .by=POF_IPN) %>%
  arrange(POF_IPN) %>%
  pull(plot) %>%
  ggarrange(plotlist=., labels='AUTO', common.legend = TRUE, legend = 'bottom')

plot_state_occupancy_by_POF_IPN
attr(plot_state_occupancy_by_POF_IPN, 'legend.text') <- 
  'State occupancy over time, A: in patients without IPN or POF, B: in patients with IPN only, C: in patients with POF only, D: in patients with both IPN and POF. Abbreviations: IPN: infectious pancreatic necrosis, POF: persistent organ failure.'

# fit linear model
fit <- lm(mean_outcome_score ~ IPN + POF, data=x)
summary(fit)


# fit ordinal model
dtmar <- data_outcome %>%
  # make sure if patient dies all remaining records are removed
  filter(
    lag(outcome_score, default=1) != 4, 
    .by=id) %>%
  mutate(yprev = lag(y), .by=id) %>%
  filter(!is.na(yprev)) %>%
  mutate(
    y = ordered(y), 
    yprev = factor(yprev, ordered=FALSE),
    day = day - min(day) + 1) %>%
  # add IPN and POF
  left_join(data_POF_IPN, by='id', relationship='many-to-one')

mod_IPN_POF <- vglm(y ~ yprev + bs(day, df = 5) + IPN + POF,
                    cumulative(reverse=TRUE, parallel=TRUE), data=dtmar)

summary(mod_IPN_POF)

table_ordinal_ORs_IPN_POF <- lapply(c('IPNYes','POFYes'), function(x){
  or <- exp(coef(mod_IPN_POF)[x] + qnorm(c(OR=.5,LL=.025,UL=.975)) * sqrt(diag(vcov(mod_IPN_POF))[x]))
  tibble(
    Variable=x,
    `OR (95% CI)` = sprintf("%.1f (%.1f-%.1f)", or[1], or[2], or[3])
  )
}) %>%
  bind_rows()

table_ordinal_ORs_IPN_POF

rm(x, dtmar, fit, mod_IPN_POF)
