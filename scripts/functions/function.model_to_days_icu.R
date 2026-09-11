model_to_days_icu <- function(...){
  model_probabilities(...) %>%
    filter(y=='ICU') %>%
    pull(prob) %>%
    sum()
}
# model_to_days_home <- function(model, time=90, OR=1, OR.add.Home=1){
#   cur_status <- tibble(
#     day = 1,
#     yprev = c('Discharged','Non-ICU ward','ICU'),
#     prob = c(0,1,0)
#   )
#   days_home <- list()
#   for(d in 1:(time-1)){
#     cur_status <- cur_status %>%
#       bind_cols(
#         predict(model, type="link", newdata=cur_status) %>%
#           as_tibble() %>%
#           mutate(
#             p_Discharged = 1 - 1 / (1 + exp(-(`logitlink(P[Y>=2])` + log(OR) + log(OR.add.Home)))),
#             `p_Non-ICU ward` = 1 - 1 / (1 + exp(-(`logitlink(P[Y>=3])` + log(OR)))) - p_Discharged,
#             p_ICU = 1 - 1 / (1 + exp(-(`logitlink(P[Y>=4])` + log(OR)))) - p_Discharged - `p_Non-ICU ward`,
#             p_Dead = 1 - p_Discharged - `p_Non-ICU ward` - p_ICU) %>%
#           select(-starts_with('logitlink'))) %>%
#       pivot_longer(starts_with('p_'), names_to = 'y', names_prefix = 'p_', values_to='prob2')
#     
#     days_home[[d]] <- cur_status %>%
#       filter(y == 'Discharged') %>%
#       summarise(p = sum(prob * prob2))
#     
#     cur_status <- cur_status %>%
#       filter(y != 'Dead') %>%
#       summarise(
#         prob = sum(prob * prob2),
#         .by=y
#       ) %>%
#       rename(yprev = y) %>%
#       mutate(day = d+1, .before=yprev)
#   }
#   days_home %>%
#     bind_rows() %>%
#     pull(p) %>%
#     sum()
# }
