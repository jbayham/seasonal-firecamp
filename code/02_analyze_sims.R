# This script goes through the simulation results found in cache/ and calculates
# metrics to graph and add to the manuscript.
# The cache directory holds simulation results and is organized by year, then
# scenario (e.g. cache/2017/baseline/ or cache/2017/low/)

# The strategy in this script is to create functions that calculate metrics for
# single run/csv, then loop through each csv applying those functions, and 
# writing the results to the output/ directory

# If you already have the `outputs/scenario_year_results.rda` files you can skip
# to the 'Extract and save results' section on line 196

library(covidfireMASS)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tidyr)
library(future)
# setwd("path/to/seasonal_Rproject")

# Load Helper functions ----
source("functions/helper_functions.R")

#################################
# Test Analysis
test <- summarise_run("cache/2017/baseline/run_1.csv")

#Check raw file
check <- read_csv("cache/2017/baseline/run_1.csv",n_max = 1000)
################################
startMessage <- function (name) {
  print(str_c("Starting '", name, "' at: ", date()))
}
endMessage <- function(name) {
  print(str_c("'", name, "' completed at: ", date()))
}

#### Running analysis in parallel ####
# If you cannot run this in parallel for any reason, just replace
# `parLapply(cl, files, summarise_run)` with `lapply(files, summarise_run)` 
# to run it sequentially
cl <- makeCluster(2)

# No Policy 2017 analysis v3
baseline_2017_files <- list.files("cache/2017/baseline", full.names = TRUE)
startMessage("Baseline 2017")
try({
  baseline_2017_results <- parLapply(cl, baseline_2017_files, summarise_run)
  save(baseline_2017_results, file = "outputs/baseline_2017_results.rda")
})
endMessage("Baseline 2017")

low_2017_files <- list.files("cache/2017/low", full.names = TRUE)
startMessage("Low 2017")
try({
  low_2017_results <- parLapply(cl, low_2017_files, summarise_run)
  save(low_2017_results, file = "outputs/low_2017_results.rda")
})
endMessage("Low 2017")

high_2017_files <- list.files("cache/2017/high", full.names = TRUE)
startMessage("High 2017")
try({
  high_2017_results <- parLapply(cl, high_2017_files, summarise_run)
  save(high_2017_results, file = "outputs/high_2017_results.rda")
})
endMessage("High 2017")

# Don't forget to close your clusters!!
stopCluster(cl)

#### Extracting and saving results ####
results_list <- c(baseline_2017_results, low_2017_results, high_2017_results)
# Worker Days Missed
map_dfr(results_list, ~get_metric("wd_missed", .x)) %>%
  group_by(run, scenario) %>%
  summarise("wd_missed" = sum(wd_missed)) %>%
  write_csv("outputs/tables/2017_all_scenarios_worker_days_missed.csv")

map_dfr(results_list, ~get_metric("cum_inf", .x)) %>%
  write_csv("outputs/tables/2017_all_scenarios_cumulative_infections.csv")

map_dfr(results_list, ~get_metric("inc_per_inc", .x)) %>%
  write_csv("outputs/tables/2017_all_scenarios_infectious_per_incident.csv")
  
map_dfr(results_list, ~get_metric("agent_reassignments", .x)) %>%
  write_csv("outputs/tables/2017_all_scenarios_infectious_reassignments.csv")

nested_outbreaks_2017 <- map_dfr(results_list, ~get_metric("nested_outbreaks", .x))
save(nested_outbreaks_2017, file = "outputs/tables/2017_all_scenarios_nested_outbreaks.rda")
