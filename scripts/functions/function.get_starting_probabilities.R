get_starting_probabilities <- function(x){
  if(min(x$day) == 0){
    res <- x %>% 
      filter(day==0) %>% 
      count(y) %>% 
      mutate(prob=n/sum(n)) %>%
      mutate(y=factor(y, levels(y), ordered=FALSE)) %>%
      select(-n)
  }else{
    res <- x %>%
      filter(day==1) %>%
      count(yprev) %>%
      mutate(prob=n/sum(n)) %>%
      select(y=yprev,prob)
  }
  res %>%
    complete(y=factor(levels(y),levels(y)), fill = list(prob=0))
}
