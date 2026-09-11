plot_state_occupancy <- function(data, model, type, OR=1, OR.mort=OR, data_only=FALSE){
  ylab <- 'Probability'
  xlab <- if(type == 'simulated') 'Days since randomization' else 'Days since admission'
  tmax <- max(data$day)
  if(type=='crude'){
    npat <- data %>% filter(day==min(day)) %>% nrow()
    title <- "Crude state occupancy probability (n=%.0f)" %>% sprintf(npat)
    plotdata <- data %>%
      as_tibble() %>%
      mutate(day = day - min(day)) %>%
      count(day, y) %>%
      mutate(prob = n / sum(n), .by=day) %>%
      select(-n)
  }else if(type=='model'){
    npat <- data %>% filter(day == min(day)) %>% nrow()
    title <- "Modeled state occupancy probability (n=%.0f)" %>% sprintf(npat)
    plotdata <- model_probabilities(model, max(data$day)+1, OR=OR, OR.mort = OR.mort, prob.day0 = get_starting_probabilities(data))
    # x <- tibble(yprev=data$y %>% levels() %>% nth(2))
    # s <- soprobMarkovOrdm(model, x, times=1:tmax, ylevels=levels(data$y),
    #                       absorb='Dead', tvarname='day')
    # plotdata <- s %>%
    #   as_tibble() %>%
    #   mutate(day = 1:tmax) %>%
    #   pivot_longer(-day, names_to='y', values_to='p') %>%
    #   mutate(y = factor(y, levels=rev(levels(data$y))))
    # if(data_only) return(plotdata)
    # p <- plotdata %>%
    #   ggplot(aes(x=factor(day), y=p, fill=y)) +
    #   geom_col() +
    #   labs(x=xlab, y=ylab, title="Modeled state occupancy probability (n=%.0f)" %>% sprintf(length(unique(data$id)))) +
    #   guides(fill=guide_legend(title='Status')) +
    #   theme(legend.position='bottom',
    #         axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
    #   scale_y_continuous(breaks=seq(0,1,.2)) +
    #   scale_x_discrete(breaks=seq(5,tmax,5))
  }else if(type=='simulation_model'){
    plotdata <- data 
    title <- "Simulated state occupancy probability"
    # }else if(type=='simulated'){
    #   plotdata <- data %>%
    #     count(day,y,tx) %>%
    #     mutate(tx = tx %>% factor(0:1, c('Control','Intervention'))) %>%
    #     mutate(
    #       p = n / sum(n), 
    #       y = factor(y, rev(ystats)),
    #       .by=c(day, tx))
    #   if(data_only) return(plotdata)
    #   p <- plotdata %>%
    #     ggplot(aes(x=factor(day), y=p, fill=y)) +
    #     geom_col() +
    #     facet_wrap(vars(tx), ncol=2) +
    #     labs(x=xlab, y=ylab, title="Simulated state occupancy probability (n=%.0f)" %>% sprintf(max(data$id))) +
    #     guides(fill=guide_legend(title='Status')) +
    #     theme(legend.position='bottom',
    #           axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +
    #     scale_y_continuous(breaks=seq(0,1,.2)) +
    #     scale_x_discrete(breaks=seq(5,tmax,5))
  }else{
    stop("Unknown argument type: ", type)
  }
  if(data_only) return(plotdata)
  plotdata %>%
    ggplot(aes(x=day, y=prob, fill=y)) +
    geom_col(position = position_stack(reverse=TRUE)) +
    guides(fill=guide_legend(title='Status')) +
    theme_minimal() +
    theme(
      legend.position='bottom', 
      axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    scale_x_continuous(breaks=seq(0,tmax,5), minor_breaks = NULL) +
    scale_y_continuous(breaks=seq(0,1,.2)) +
    scale_fill_manual(values=palette.colors()[-1]) +
    labs(x=xlab, y=ylab, title=title)
}
