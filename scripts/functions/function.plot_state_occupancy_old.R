plot_state_occupancy_old <- function(data, model, type){
  ylab <- 'Probability'
  xlab <- if(type == 'simulated') 'Days since randomization' else 'Days since admission'
  tmax <- max(data$day)
  if(type=='crude'){
    p <- data %>%
      mutate(day = day - min(day) + 1) %>%
      propsPO(y ~ day, data=.) +
      guides(fill=guide_legend(title='Status')) +
      theme(legend.position='bottom', axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
      scale_x_discrete(breaks=seq(5,tmax,5)) +
      scale_y_continuous(breaks=seq(0,1,.2)) +
      labs(x=xlab, y=ylab, title="Crude state occupancy probability (n=%.0f)" %>% sprintf(length(unique(data$id))))
  }else if(type=='model'){
    x <- tibble(yprev=data$y %>% levels() %>% nth(2))
    s <- soprobMarkovOrdm(model, x, times=1:tmax, ylevels=levels(data$y),
                          absorb='Dead', tvarname='day')
    tmin = min(data$day)
    p <- s %>%
      as_tibble() %>%
      mutate(day = tmin:tmax) %>%
      pivot_longer(-day, names_to='y', values_to='p') %>%
      mutate(y = factor(y, levels=rev(levels(data$y)))) %>%
      ggplot(aes(x=factor(day), y=p, fill=y)) +
      geom_col() +
      labs(x=xlab, y=ylab, title="Modeled state occupancy probability (n=%.0f)" %>% sprintf(length(unique(data$id)))) +
      guides(fill=guide_legend(title='Status')) +
      theme(legend.position='bottom',
            axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
      scale_y_continuous(breaks=seq(0,1,.2)) +
      scale_x_discrete(breaks=seq(5,tmax,5))
  }else if(type=='simulated'){
    p <- data %>%
      count(day,y,tx) %>%
      mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
      mutate(
        p = n / sum(n), 
        y = factor(y, rev(ystats)),
        .by=c(day, tx)) %>%
      ggplot(aes(x=factor(day), y=p, fill=y)) +
      geom_col() +
      facet_wrap(vars(tx), ncol=2) +
      labs(x=xlab, y=ylab, title="Simulated state occupancy probability (n=%.0f)" %>% sprintf(max(data$id))) +
      guides(fill=guide_legend(title='Status')) +
      theme(legend.position='bottom',
            axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
      scale_y_continuous(breaks=seq(0,1,.2)) +
      scale_x_discrete(breaks=seq(5,tmax,5))
  }
  p
}
