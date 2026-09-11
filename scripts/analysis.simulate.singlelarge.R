# control group
model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR.control)
# intervention group assuming proportional odds
model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR.control * 0.85)
# intervention group assuming non-proportional odds (smaller effect for mortality)
model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR.control * 0.85, OR.mort = 1.23)

trial1 <- simulate_trial(model = fit_ab_drainage, n.group = 10000, 
                         ystats = data_outcome_ab_drainage$y %>% levels(), futime = futime-1, 
                         OR.intervention = 0.85 * LOOR.control, OR.control = LOOR.control)
trial2 <- simulate_trial(model = fit_ab_drainage, n.group = 10000, 
                         ystats = data_outcome_ab_drainage$y %>% levels(), futime = futime-1, 
                         OR.intervention = 0.85 * LOOR.control, OR.control = LOOR.control, 
                         OR.intervention.mort = 1.23)

# mortality risk difference with proportional odds

trial1 %>%
  summarise(died = any(y=='Dead'), .by=c(id, tx)) %>%
  summarise(
    n = n(),
    y = sum(died),
    pct = "%.0f (%.1f%%)" %>% sprintf(sum(died), mean(died)*100), 
    .by=tx) %>%
  mutate(
    RD = if_else(tx==0, '', "%.1f%%" %>% sprintf((y[tx==1]/n[tx==1] - y[tx==0]/n[tx==0])*100)),
    `95% ci` = if_else(tx==0, '', PropCIs::diffscoreci(y[tx==1], n[tx==1], y[tx==0], n[tx==0], .95)$conf.int %>%
      {sprintf("(%.1f to %.1f)", .[1]*100, .[2]*100)}))

model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR.control * 0.85) - model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR.control)

# mortality risk difference with non-proportional odds

trial2 %>%
  summarise(died = any(y=='Dead'), .by=c(id, tx)) %>%
  summarise(
    n = n(),
    y = sum(died),
    pct = "%.0f (%.1f%%)" %>% sprintf(sum(died), mean(died)*100), 
    .by=tx) %>%
  mutate(RD = if_else(tx==0, '', "%.1f%%" %>% sprintf((y[tx==1]/n[tx==1] - y[tx==0]/n[tx==0])*100)),
         `95% ci` = if_else(tx==0, '', PropCIs::diffscoreci(y[tx==1], n[tx==1], y[tx==0], n[tx==0], .95)$conf.int %>%
                              {sprintf("(%.1f to %.1f)", .[1]*100, .[2]*100)}))

model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR.control * 0.85, OR.mort = 1.23) - model_to_mort(model=fit_ab_drainage, futime=90, OR=LOOR.control)

# what is the relative effect for any ICU admission? And for ICU admission >2 days? And >7 days?
# use trial2, it has the non-proportional assumption for mortality and LOOR of 0.85
trial2 %>%
  summarise(any_ICU_stay = any(y == 'ICU'), .by=c(id,tx)) %>%
  summarise(mean_any_ICU_stay = mean(any_ICU_stay), .by=tx) %>%
  mutate(RR = if_else(tx==0, NA_real_, mean_any_ICU_stay[tx==1] / mean_any_ICU_stay[tx==0]))

trial2 %>%
  summarise(ICU_stay_gt2 = sum(y == 'ICU') > 2, .by=c(id,tx)) %>%
  summarise(mean_ICU_stay_gt2 = mean(ICU_stay_gt2), .by=tx) %>%
  mutate(RR = if_else(tx==0, NA_real_, mean_ICU_stay_gt2[tx==1] / mean_ICU_stay_gt2[tx==0]))

trial2 %>%
  summarise(ICU_stay_gte7 = sum(y == 'ICU') >= 7, .by=c(id,tx)) %>%
  summarise(mean_ICU_stay_gte7 = mean(ICU_stay_gte7), .by=tx) %>%
  mutate(RR = if_else(tx==0, NA_real_, mean_ICU_stay_gte7[tx==1] / mean_ICU_stay_gte7[tx==0]))

# what is the relative effect for number of admission days for those alive on day 90?
trial2 %>%
  filter(!any(y=='Dead'), .by=id) %>%
  summarise(days_admitted = sum(y != 'Discharged'), .by=c(id,tx)) %>%
  summarise(mean_days_admitted = mean(days_admitted), .by=tx) %>%
  mutate(diff = if_else(tx==0, NA_real_, mean_days_admitted[tx==1] - mean_days_admitted[tx==0]))

# same for days alive discharged (in all subjects; cave, not same population as previous number)
trial2 %>%
  summarise(days_alive_discharged = sum(y == 'Discharged'), .by=c(id,tx)) %>%
  summarise(mean_days_alive_discharged = mean(days_alive_discharged), .by=tx) %>%
  mutate(diff = if_else(tx==0, NA_real_, mean_days_alive_discharged[tx==1] - mean_days_alive_discharged[tx==0]))
