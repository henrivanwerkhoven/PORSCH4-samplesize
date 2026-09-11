plot_transitions <- function(data){
  dt <- data %>%
    # make sure if patient dies all remaining records are removed
    filter(lag(status, default=2) != 3, .by=id)
  
  # show transitions
  propsTrans(y ~ day + id, data=dt, maxsize=3, arrow='->') +
    theme(axis.text.x=element_text(angle=90, hjust=1))
}