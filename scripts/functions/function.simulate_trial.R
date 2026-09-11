simulate_trial <- function(model, n.group, ystats, futime, OR.intervention, OR.control=1, OR.add.home=1/OR.control, OR.intervention.mort=OR.intervention){
  simdat <- simdat.next <- tibble(
    id = 1:(n.group*2),
    day = 0,
    tx = rep(0:1, n.group),
    y = factor(ystats[2], ystats)
  )
  for(d in 1:futime){
    simdat.pred <- simdat.next %>%
      mutate(yprev = y, day=d) %>%
      dplyr::select(-y)
    simdat.now <- predict(model, type="link", newdata=simdat.pred) %>%
      as_tibble() %>%
      mutate(
        tx = simdat.next$tx,
        # p12 = 1 - 1 / (1 + exp(-(`logitlink(P[Y>=2])` + log(OR.control) * (1-tx) + log(OR.intervention) * tx + log(OR.add.home)))),
        p12 = 1 - 1 / (1 + exp(-(`logitlink(P[Y>=2])` + log(OR.control) * (1-tx) + log(OR.intervention) * tx))),
        p23 = 1 - 1 / (1 + exp(-(`logitlink(P[Y>=3])` + log(OR.control) * (1-tx) + log(OR.intervention) * tx))),
        p34 = 1 - 1 / (1 + exp(-(`logitlink(P[Y>=4])` + log(OR.control) * (1-tx) + log(OR.intervention.mort) * tx))),
        r = runif(n()),
        y = case_when(
          r < p12 ~ ystats[1],
          r < p23 ~ ystats[2],
          r < p34 ~ ystats[3],
          TRUE ~ ystats[4]) %>%
          factor(ystats)) %>%
      dplyr::select(y)
    simdat.next <- bind_cols(simdat.pred, simdat.now)
    simdat <- bind_rows(simdat, simdat.next)
    # exclude those with absorbing state dead
    simdat.next <- simdat.next %>%
      filter(y != ystats[4])
  }
  
  # exclude randomisation day and make y ordinal
  simdat <- simdat %>%
    filter(day > 0) 
  simdat
}
