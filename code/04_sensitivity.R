# Running 3 scenarios, 3 simulations per run
# The working directory should be `seasonal_Rproject`, you can do this
# by loading the `seasonal_Rproject.Rproject` file or
# setwd("path/to/seasonal_Rproject)

library(covidfireMASS)
library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(furrr)
# setwd("path/to/seasonal_Rproject")

# Load Helper functions ----
source("functions/helper_functions.R")
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

#eir - prob acquire infection off of fire
low_parms_2017_eir <- base_parms_2017
low_parms_2017_eir$scenario = "Low Off-Fire Infection"
low_parms_2017_eir$eir = 0.00084
high_parms_2017_eir <- base_parms_2017
high_parms_2017_eir$scenario = "High Off-Fire Infection"
high_parms_2017_eir$eir = 0.00021

#pIQ - prob symptomatic does not quarantine
low_parms_2017_pIQ <- base_parms_2017
low_parms_2017_pIQ$scenario = "Low Self-Isolation"
low_parms_2017_pIQ$pIQ = 0.3
high_parms_2017_pIQ <- base_parms_2017
high_parms_2017_pIQ$scenario = "High Self-Isolation"
high_parms_2017_pIQ$pIQ = 0.7

#Maybe mess with vax plan??? (modify in 00_make_sim_inputs)


parm_list <- list(base_parms_2017,low_parms_2017_eir,high_parms_2017_eir,low_parms_2017_pIQ,high_parms_2017_pIQ)

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
  
  #Summarize info for infections
  sum_inf <- summarise_inf(out)$cum_inf
  
  #return(sum_inf)
  
  save_path <- str_c("cache/", parms[["year"]], "/", parms[["scenario"]])
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
  }
  write_csv(sum_inf, str_c(save_path, "/run_", run_num, ".csv"))
  return(invisible(NULL))
}


#### Running simulations (in parallel) ####
#parm_set=parm_list[[1]]
plan(multisession(workers = 18))
sens_result <- map(parm_list,function(parm_set){
  suppressWarnings(future_map(c(1:nruns),~single_sim(.,parm_set),.progress = T,.options=furrr_options(seed=T)))
})
  
results <- dir("cache/2017/baseline/",full.names = T) %>%
  map_dfr(read_csv)


plot_cum_inf(results)

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
