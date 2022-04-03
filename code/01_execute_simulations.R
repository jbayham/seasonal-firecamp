# Running 3 scenarios, 3 simulations per run
# The working directory should be `seasonal_Rproject`, you can do this
# by loading the `seasonal_Rproject.Rproject` file or
# setwd("path/to/seasonal_Rproject)

library(covidfireMASS)
library(readr)
library(dplyr)
library(stringr)
library(parallel)

# Load Data ----
load("inputs/sim_inputs_2017_complete.rda")

# Simulation Parameters ----
nruns <- 100
seed0 <- 2345
num_cores <- 2
working_directory <- getwd()

# Scenario Parameters ----
base_parms_2017 <- list("scenario" = "baseline",
                        "year"     = "2017",
                        "inc_data" = inc_id_2017,
                        "mod_data" = mod_id_2017,
                        "inc_info" = inc_info_2017,
                        "vax_df"   = vax_plan_base_2017,
                        "overhead_ids" = overhead_ids_2017,
                        "BI"  = 0.13,
                        "eir" = 0.00042,
                        "pIQ" = 0.5)
low_parms_2017 <- list("scenario" = "low",
                        "year"     = "2017",
                        "inc_data" = inc_id_2017,
                        "mod_data" = mod_id_2017,
                        "inc_info" = inc_info_2017,
                        "vax_df"   = vax_plan_low_2017,
                        "overhead_ids" = overhead_ids_2017,
                        "BI"  = 0.177,
                        "eir" = 0.00084,
                        "pIQ" = 0.3)
high_parms_2017 <- list("scenario" = "high",
                        "year"     = "2017",
                        "inc_data" = inc_id_2017,
                        "mod_data" = mod_id_2017,
                        "inc_info" = inc_info_2017,
                        "vax_df"   = vax_plan_high_2017,
                        "overhead_ids" = overhead_ids_2017,
                        "BI"  = 0.077,
                        "eir" = 0.00021,
                        "pIQ" = 0.7)

#### Single simulation function ####
# This function will try to write directories to place results if they don't already exist
# the directories will look like this: `cache/<year>/<scenario>/`
single_sim <- function(run_num, parms) {
  set.seed(seed0 + run_num)
  out <- seasonal_sim(inc_data = parms[["inc_data"]],
                      mod_data = parms[["mod_data"]],
                      inc_info = parms[["inc_info"]],
                      overhead_ids = parms[["overhead_ids"]],
                      module_multiplier = 4,
                      leads  = 4,
                      BI     = parms[["BI"]],
                      eir    = parms[["eir"]],
                      vax_df = parms[["vax_df"]],
                      pIQ    = parms[["pIQ"]],
                      R_init = 0.25,
                      I_init = 0.00598,
                      raw   = TRUE) %>%
    mutate("run" = run_num,
           "scenario" = parms[["scenario"]])
  
  save_path <- str_c("cache/", parms[["year"]], "/", parms[["scenario"]])
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  } 
  write_csv(out, str_c(save_path, "/run_", run_num, ".csv"))

  return(0)
}

#### Running simulations sequentially ####
sapply(1:nruns, single_sim, parms = base_parms_2017)
sapply(1:nruns, single_sim, parms = low_parms_2017)
sapply(1:nruns, single_sim, parms = high_parms_2017)

#### Running simulation with parallel ####
# This section is for running the simulations in parallel.
# Remember `type = FORK` only works in linux, omit the `type` option
# if you are on windows

# cl <- makeCluster(num_cores) # creating the cluster
# clusterExport(cl, varlist = c("seed0", "working_directory"))
# clusterEvalQ(cl, {
#   library(covidfireMASS)
#   library(dplyr)
#   library(readr)
#   library(stringr)
#   # setwd(working_directory)
# })
# 
# try({
#   parSapply(cl, 1:nruns, single_sim, parms = base_parms_2017)
# }, silent = FALSE)
# 
# try({
#   parSapply(cl, 1:nruns, single_sim, parms = low_parms_2017)
# }, silent = FALSE)
# 
# try({
#   parSapply(cl, 1:nruns, single_sim, parms = high_parms_2017)
# }, silent = FALSE)
# 
# stopCluster(cl)
