# set follow-up time
futime <- 90

# packages, scripts, load and clean data
source("scripts/init.R")

# analyses for the power calculation
source("scripts/analysis.descriptive.R")
source("scripts/analysis.model.fitting.R")
source("scripts/analysis.model.output.R")

# define scenarios
source("scripts/analysis.scenarios.R")

# # run simulations (very time consuming)
# source("scripts/analysis.simulate.singlelarge.R")
# source("scripts/analysis.simulation.multiple.R")

# alternative: load simulations from cache (most recent one that is based on same futime and cache_filename)
source("scripts/analysis.simulation.multiple.fromcache.R")

# visualize the simulated scenarios
source("scripts/analysis.simulation.plot.trajectories.R")

# calculate the impact of LOORs on individual components
source("scripts/analysis.simulation.components.impact.R")

# perform calculation of the power
# for individually randomized controlled trial
source("scripts/analysis.powercalculation.R")

# for stepped-wedge trial
source("scripts/analysis.samplesize_swcrt.R")

# output of power calculation
source("scripts/output.powercalculation.R")
