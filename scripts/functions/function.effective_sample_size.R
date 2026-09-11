# functions to calculate power for a specific sample size based on simulations of multiple sample size simulations
# or calculate sample size for a specific power based on the same
# alpha is one-sided here!
get_V_proxy <- function(n, power, alpha){
  opt <- optimise(function(V){
    f <- (qnorm(alpha) + qnorm(1-power)) ^ 2
    v_ <- n / f
    abs(sum((v_-V)^2))
  }, c(0,1e9))
  opt$minimum
}
invert_power <- function(n, V, alpha){
  power <- pnorm(sqrt(n/V) + qnorm(alpha))
  power
}
invert_N <- function(V, power, alpha){
  f <- (qnorm(alpha) + qnorm(1-power)) ^ 2
  f * V
}

effective_sample_size <- function(power, power2, alpha){
  V <- get_V_proxy(1, power, alpha)
  V2 <- get_V_proxy(1, power2, alpha)
  V2 / V
}
