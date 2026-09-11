# packages
lib <- function(pkg){
  if(!require(pkg, char=TRUE)){
    install.packages(pkg)
    library(pkg)
  }
}
lib('MASS')
lib('tidyverse')
lib('ggpubr')
lib('readxl')
lib('haven')
lib('cmprsk')
lib('intccr')
lib('rms')
lib('VGAM')
lib('rmsb')
lib('furrr')
lib('progressr')
lib('rmarkdown')
lib('gt')
rm(lib)
# if(!require(cmdstanr)){
#   install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#   require(cmdstanr)
# }



# cache filename
cache_filename <- paste0("scenarios_patient_days_total_allresections_",
                         futime,
                         "d_",
                         Sys.Date(),
                         ".rds")

# get data 
source('scripts/load_data.R')

# load all functions
for(script in list.files('scripts/functions', '^function.+\\.R$',full.names = TRUE)){
  source(script)
}

rm(script)
