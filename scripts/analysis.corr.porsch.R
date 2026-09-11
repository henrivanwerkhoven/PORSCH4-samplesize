# these data are not being used elsewhere, so I load them here.
data_door <- read_xlsx("data/dataset_allresections_ABdrain_wide_2026-06-03.xlsx") %>%
  rename(door = `complicatie_statuscategorie\r\n1= overleden\r\n2= orgaanfalen + bloeding\r\n3= orgaanfalen, zonder bloeding\r\n4 = bloeding, zonder orgaanfalen\r\n5= geen van bovenstaande`) %>%
  mutate(door = 6 - door)

# function to calculate correlation between mean loor and door with 95% ci.
get_corr <- function(corr_data, method="spearman", b=1000){  
  c(
    corr=with(corr_data, cor(mean_loor, ranks, method = method)),
    sapply(1:b, function(i){
      with(corr_data %>% slice_sample(prop=1, replace=TRUE), cor(mean_loor, ranks, method = method))
    }) %>%
      quantile(c(.025,.975)))
}

corr_data <- data_outcome_ab_drainage %>%
  summarise(mean_loor = mean(outcome_score), .by=id) %>%
  inner_join(data_door, by=join_by(id == Record.Id))

plot_mean_loor_vs_door <- corr_data %>%
  ggplot(aes(x=mean_loor, y=door)) +
  geom_jitter(width=0, height=.3) +
  labs(x="Mean longitudinal ordinal outcome for 90 days", y="Desirability of outcome ranking at day 90")

spearman_mean_loor_vs_door <- get_corr(corr_data %>% rename(ranks = door))

plot_mean_loor_vs_porsch <- corr_data %>%
  ggplot(aes(x=mean_loor, y=door %>% factor(1:5, c('No','Yes','Yes','Yes','Yes')))) +
  geom_jitter(width=0, height=.3) +
  labs(x="Mean longitudinal ordinal outcome for 90 days", y="PORSCH endpoint")

spearman_mean_loor_vs_porsch <- get_corr(corr_data %>% mutate(ranks = door != 1))

plot_mean_loor_vs_OFD <- corr_data %>%
  ggplot(aes(x=mean_loor, y=door %>% factor(1:5, c('No','No','Yes','Yes','Yes')))) +
  geom_jitter(width=0, height=.3) +
  labs(x="Mean longitudinal ordinal outcome for 90 days", y="Organ failure or death")

spearman_mean_loor_vs_OFD <- get_corr(corr_data %>% mutate(ranks = door > 2))

plot_mean_loor_vs_death <- corr_data %>%
  ggplot(aes(x=mean_loor, y=door %>% factor(1:5, c('No','No','No','No','Yes')))) +
  geom_jitter(width=0, height=.3) +
  labs(x="Mean longitudinal ordinal outcome for 90 days", y="Death")

spearman_mean_loor_vs_death <- get_corr(corr_data %>% mutate(ranks = door == 5))



state_occupancy_by_door <- data_outcome_ab_drainage %>%
  inner_join(data_door, by=join_by(id == Record.Id)) %>%
  arrange(door) %>%
  summarise(
    x = (
      plot_state_occupancy(pick(everything()), type = 'crude') +
        labs(subtitle = sprintf('Door rank = %.0f', door))) %>% list(),
    .by=door)

state_occupancy_by_door$x


state_occupancy_by_porsch <- data_outcome_ab_drainage %>%
  inner_join(data_door, by=join_by(id == Record.Id)) %>%
  mutate(porsch = (1 * (door >= 2)) %>% factor(0:1,c('No','Yes'))) %>%
  arrange(porsch) %>%
  summarise(
    x = (
      plot_state_occupancy(pick(everything()), type = 'crude') +
        labs(subtitle = sprintf('PORSCH outcome: %s', porsch))) %>% list(),
    .by=porsch)

state_occupancy_by_porsch$x

state_occupancy_by_organfailure <- data_outcome_ab_drainage %>%
  inner_join(data_door, by=join_by(id == Record.Id)) %>%
  mutate(OFD = (1 * (door >= 3)) %>% factor(0:1,c('No','Yes'))) %>%
  arrange(OFD) %>%
  summarise(
    x = (
      plot_state_occupancy(pick(everything()), type = 'crude') +
        labs(subtitle = sprintf('Organ failure or death: %s', OFD))) %>% list(),
    .by=OFD)

state_occupancy_by_organfailure$x

# generate report
output_file <- "%s/output/PORSCH_AB_correlations_LOOR_DOOR_PORSCH_%s" %>% 
  sprintf(getwd(), 
          Sys.Date())
render("scripts/PORSCH-AB_output_correlations.Rmd", output_file=output_file, output_format = "html_document")
rm(output_file)

