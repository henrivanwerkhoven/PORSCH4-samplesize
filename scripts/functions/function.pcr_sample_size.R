## sample size calculation for parallel cluster-randomized trial - calculate the design effect

# Eldridge et al. Practical considerations for sample size calculation for cluster randomized trials. 
# Journal of Epidemiology and Population Health 2024, 72:1. doi: 10.1016/j.jeph.2024.202198

# size_rct = size of 1 arm assuming 1:1 randomized trial
# icc = intra-cluster correlation coefficient
# k = number of clusters
pcr_sample_size <- function(size_rct, icc, k){
  p <- icc
  # find n (total size) for which is the product of the design effect and 2x size_rct
  n <- optimize(function(n){
    M <- n / k
    DE <- 1 + (M-1)*p
    size_sw_needed <- DE * 2 * size_rct
    abs(size_sw_needed - n)
  }, interval=c(size_rct-1, 99999))$minimum
  if(n < size_rct) n <- 99999
  data.frame(size_rct=size_rct,icc=icc,k=k,cluster_size=n/k, total_size=n, DE=1 + (n/k) * icc)
}
