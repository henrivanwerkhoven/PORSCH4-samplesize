model_probabilities <- function(model, futime, OR=1, prob.day0=NULL, keep.yprev=FALSE, OR.mort=OR){
  ystats <- model@misc$ynames %>% factor(.,.)
  if(is.null(prob.day0)){
    prob.day0 <- tibble(
      y = ystats[-length(ystats)],
      prob = c(0,1,0))
  }
  data <- expand_grid(
    day = 1:(futime-1),
    yprev = ystats[-length(ystats)]
  ) %>%
    {
      bind_cols(
        ., 
        predictvglm(model, newdata=.) %>%
          as_tibble())
      } %>%
    mutate(
      # to do: make this generic to allow more than 4 states
      p_Discharged = 1 - plogis(`logitlink(P[Y>=2])` + log(OR)),
      `p_Non-ICU ward` = 1 - plogis(`logitlink(P[Y>=3])` + log(OR)) - p_Discharged,
      p_ICU = 1 - plogis(`logitlink(P[Y>=4])` + log(OR.mort)) - p_Discharged - `p_Non-ICU ward`,
      p_Dead = 1 - p_Discharged - `p_Non-ICU ward` - p_ICU) %>%
    select(-starts_with('logitlink')) %>%
    pivot_longer(starts_with('p_'), names_to = 'y', names_prefix = 'p_', values_to='prob2') %>%
    mutate(y = factor(y, levels(ystats))) %>%
    # add day 0 probability
    bind_rows(prob.day0 %>% mutate(day=0))
  # calculate per day
  for(d in 1:(futime-1)){
    data <- bind_rows(
      data %>% 
        filter(day != d),
      data %>%
        filter(day == d) %>%
        left_join(
          data %>%
            filter(day==d-1) %>%
            select(yprev=y, prob) %>%
            summarise(probprev = sum(prob), .by=yprev),
          by="yprev") %>%
        mutate(prob = probprev * prob2) %>%
        select(-probprev)
    )
  }
  if(keep.yprev){
    data <- data %>%
      select(day, y, yprev, prob) %>%
      # for state Dead, since this is an absorbing state, the daily probability is the proportion of initial population entering that state
      # --> use cumsum to solve it
      mutate(prob = if_else(y == 'Dead', cumsum(prob), prob), .by=c(y, yprev))
  }else{
    data <- data %>%
      summarise(prob = sum(prob), .by=c(day,y)) %>%
      # for state Dead, since this is an absorbing state, the daily probability is the proportion of initial population entering that state
      # --> use cumsum to solve it
      mutate(prob = if_else(y == 'Dead', cumsum(prob), prob), .by=c(y))
  }
  data 
}
