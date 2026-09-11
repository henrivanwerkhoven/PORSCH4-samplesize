plot_power <- function(data, pvars='power', plabels=NULL, nvar='N', fvar=NULL, alpha=.025){
  ps <- syms(pvars)
  p_orgs <- syms(paste0(pvars,'_org'))
  p_invs <- syms(paste0(pvars,'_inv'))
  n <- sym(nvar)
  if(!is.null(fvar)) f <- sym(fvar)
  for(i in 1:length(pvars)){
    p <- ps[[i]]
    p_org <- p_orgs[[i]]
    p_inv <- p_invs[[i]] 
    if(!is.null(fvar)){
      data <- data %>%
        rename_with(function(x) paste0(x, '_org'), .cols = !!p) %>%
        mutate({{p_inv}} := invert_power(!!n, get_V_proxy(!!n, !!p_org, alpha), alpha), .by=!!f)
    }else{
      data <- data %>%
        rename_with(function(x) paste0(x, '_org'), .cols = !!p) %>%
        mutate({{p_inv}} := invert_power(!!n, get_V_proxy(!!n, !!p_org, alpha), alpha))
    }
  }
  data <- data %>%
    pivot_longer(cols = starts_with(pvars), names_pattern = '^(.+)_([a-z]{3})$', names_to = c('model','.value'))
  if(!is.null(plabels)){
    data <- data %>%
      mutate(model = factor(model, names(plabels), plabels))
  }
  p <- data %>%
    ggplot(aes(x=!!n, linetype=model, shape=model)) +
    geom_point(aes(y=org)) +
    geom_line(aes(y=inv)) +
    labs(x='Sample size per treatment arm', y='Power', linetype="Analysis", shape="Analysis") +
    theme(legend.position = 'bottom') +
    scale_y_continuous(breaks=seq(0,1,.2), limits = c(0,1))
  if(!is.null(fvar)){
    p <- p + 
      facet_wrap(vars(!!f), scales = 'free_x', dir = 'v')
  }
  p
}

