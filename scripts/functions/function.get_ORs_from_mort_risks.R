get_ORs_from_mort_risks <- function(model, time, risk.intervention, risk.control=NULL){
  # first derive which OR should be taken to get a 90-day mortality of risk.placebo (if not NULL) 
  # This is the risk expected in the placebo group.
  # Note that I want to speed at which patients (while in the hospital) are 
  # discharged not to be changed compared to the observational data. Hence I 
  # include 1/OR as 4th argument in model_to_mort.
  if(!is.null(risk.control)){
    opt <- optimize(function(OR){
      p <- model_to_mort(model, time, OR, 1/OR)
      abs(p - risk.control)
    }, c(1e-4, 1/1e-4), tol=.001)
    OR.control <- opt$minimum
  }else{
    OR.control <- 1
  }
  
  # next derive which OR should be taken to get a 90-day mortality of risk.intervention
  # This is the risk hypothesised in the intervention group
  # Here I include 1/OR.placebo as 4th argument, so that rate of discharge *is* 
  # changed depending on the OR for phage therapy.
  opt <- optimize(function(OR){
    p <- model_to_mort(model, time, OR, 1/OR.control)
    abs(p - risk.intervention)
  }, c(1e-4, OR.control - 1e-4), tol=.001)
  OR.intervention <- opt$minimum
  
  list(control = OR.control, intervention = OR.intervention)
}
