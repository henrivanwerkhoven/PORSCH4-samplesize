# simulate multiple trials

if(!exists("scenarios")){
  stop("Run analysis.scenarios.R first to define the scenarios.")
}

scenarios <- scenarios %>%
  mutate(pow_sim = list(list())) 

ystats <- data_outcome_ab_drainage$y %>% levels()

iter <- 200

for(i in 1:nrow(scenarios)){
  message('Running scenario ', i, '/', nrow(scenarios), ': N=', scenarios[i,'N'], '; LOOR=', scenarios[i,'LOOR'])
  
  scenarios$pow_sim[[i]] <- with(
    slice(scenarios, i),
    power_simulation(model = fit_ab_drainage, n.group=round(NiRCT), ystats=ystats, futime=futime-1, 
                     OR.intervention = LOOR_int, OR.control = LOOR_ctr, OR.intervention.mort = LOOR_int_mort, 
                     iter=iter, seed=123+i))
}
rm(i, iter)

saveRDS(scenarios, paste0("cache/",cache_filename))
