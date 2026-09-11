## sample size calculation stepped-wedge trial - calculate the design effect

# Woertman W, De hoop E, Moerbeek M, Zuidema SU, Gerritsen DL, Teerenstra S. Stepped wedge designs could reduce the required sample size in cluster randomized trials. J Clin Epidemiol. 2013;66(7):752-8.
# NB: the referred paper contains an error. To get the design effect multiply this formula by (k+1). (Personal communicatino E. de Hoop)
# NB: does not allow inclusion of wash-in period
# NB: assumes equal cluster sizes

# size_rct = size of 1 arm assuming 1:1 randomized trial
# icc = intra-cluster correlation coefficient
# k = number of steps (assuming at each step one cluster crosses over to the intervention)
swcr_sample_size <- function(size_rct, icc, k){
  n <- optimize(function(n){
    DE <- swcr_design_effect(n, icc, k)
    size_sw_needed <- size_rct * DE * 2
    size_sw_realised <- k * (1 + k) * n
    abs(size_sw_needed - size_sw_realised)
  }, interval=c(1,size_rct))$minimum
  data.frame(size_rct=size_rct,icc=icc,k=k,cluster_size=n, total_size=n*k*(k+1), DE = n*k*(k+1) / (size_rct * 2))
}

swcr_design_effect <- function(n, p, k, t=1, b=1){
  ktn <- k * t * n
  bn <- b * n
  DE <- (1 + p * (ktn+bn-1)) / (1 + p * (0.5 * ktn + bn - 1)) * (3 *(1-p)) / (2 * t * (k-1/k)) * (k+1)
  DE
}
