power_simulation <- function(model, n.group, ystats, futime, OR.intervention, OR.control=1, OR.add.home=1/OR.control, OR.intervention.mort=OR.intervention, iter=50, seed=123){
  retval <- list(
    OR.intervention = OR.intervention, 
    OR.control = OR.control,
    OR.add.home = OR.add.home,
    OR.intervention.mort = OR.intervention.mort,
    n.group = n.group,
    iter = iter,
    seed = seed,
    delta_mort = c(),
    delta_home = c(),
    OR.est.long = list(),
    OR.est.days = list(),
    p.wmu.days = c()
  )
  
  plan(multisession, workers=future::availableCores(logical = FALSE)-1)  # Use multiple sessions for parallel processing
  with_progress({
    iters <- 1:iter
    p <- progressor(along = iters) # Create a progressor
    trials <- future_map(
      iters, 
      ~ {
        dplyr::select
        # simulate a trial trials
        simdat <- simulate_trial(model = model, n.group = n.group, ystats = ystats, futime = futime,
                                 OR.intervention = OR.intervention, OR.control = OR.control, OR.add.home = OR.add.home, OR.intervention.mort=OR.intervention.mort)
        
        # fit model
        
        simdatreg <- simdat %>%
          mutate(y = ordered(y))
        
        fit.sim <- vglm(y ~ yprev + bs(day, df=4) + tx,
                        cumulative(reverse=TRUE, parallel=TRUE), data=simdatreg)

        # estimated OR
        OR.est.long <- exp(coef(fit.sim)['tx'] + c(OR=0 ,OR.lower=-1.96, OR.upper=1.96) * sqrt(diag(vcov(fit.sim)))['tx'])

        # mortality benefit
        delta_mort <- simdat %>% 
          summarise(dead = any(y=='Dead'), .by=c(id,tx)) %>%
          summarise(
            mort = sum(dead),
            pct = mean(dead),
            .by=tx) %>%
          mutate(
            # RR = if_else(tx == 1, pct[tx==1] / pct[tx==0], NA_real_),
            RD = if_else(tx == 1, pct[tx==1] - pct[tx==0], NA_real_)
          )
        
        # increase in days alive out of hospital
        delta_home <- simdat %>%
          summarise(days_discharged_alive = sum(y == 'Discharged'), .by=c(id, tx)) %>%
          summarise(mean_days_discharged_alive = mean(days_discharged_alive), .by=tx) %>%
          mutate(diff = if_else(tx == 1, diff(mean_days_discharged_alive), NA_real_))
        
        # alternative outcome measure days alive out of the hospital
        days_alive <- simdat %>%
          summarise(
            days_alive_out_of_hospital = if_else(
              any(y=='Dead'),
              -1,
              sum(y == 'Discharged')
            ),
            .by=c(id,tx)
          ) %>%
          mutate(
            y = days_alive_out_of_hospital %>%
              ordered(-1:futime)
          )
        
        mwu <- with(days_alive, wilcox.test(days_alive_out_of_hospital ~ tx))
        
        fit.days <- polr(y ~ tx, data=days_alive, Hess=TRUE)
        OR.est.days <- exp(coef(fit.days) + c(OR=0,OR.lower=-1.96,OR.upper=1.96) * sqrt(diag(vcov(fit.days))['tx']))
        OR.est.days[c('beta','se')] <- c(coef(fit.days), sqrt(diag(vcov(fit.days))['tx']))
        
        p() # progress bar
        
        list(
          OR.est.long = OR.est.long,
          OR.est.days = OR.est.days,
          p.wmu.days = mwu$p.value,
          delta_mort = na.omit(delta_mort$RD),
          delta_home = na.omit(delta_home$diff)
        )
      }, 
    .options = furrr_options(seed = seed))
  })
  plan(sequential)
  
  retval$OR.est.long <- sapply(trials, function(x) x$OR.est.long)
  retval$OR.est.days <- sapply(trials, function(x) x$OR.est.days)
  retval$p.wmu.days  <- sapply(trials, function(x) x$p.wmu.days)
  retval$delta_mort  <- sapply(trials, function(x) x$delta_mort)
  retval$delta_home  <- sapply(trials, function(x) x$delta_home)
  
  retval
}
