

library(covidfireMASS)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(tidyr)
library(future)
# setwd("path/to/seasonal_Rproject")

# Helper functions ----
wd_missed <- function(df) {
  df %>%
    filter(quarantine & inc_id > 0) %>%
    count(state, vaccinated, name = "wd_missed") %>%
    mutate("scenario" = df[["scenario"]][1],
           "run"      = df[["run"]][1])
}

summarise_inf <- function(df) {
  scenario <- df[["scenario"]][1]
  run <- df[["run"]][1]
  
  res_id_split <- df %>%
    filter(state == "E") %>%
    group_split(res_id)
  
  exposure_info <- res_id_split %>%
    map(function(res_df) {
      index_exposed <- which.min(res_df[["time"]])
      date_exposed <- res_df[["time"]][index_exposed]
      inc_exposed <- res_df[["inc_id"]][index_exposed]
      loc_exposed <- ifelse(inc_exposed > 0, "On Fire", "Off Fire")
      list("res_id"       = res_df[["res_id"]][1],
           "date_exposed" = date_exposed,
           "inc_exposed"  = inc_exposed,
           "loc_exposed"  = loc_exposed)
    })
  
  # cumulative infections
  cum_inf <- exposure_info %>%
    bind_rows() %>%
    count(loc_exposed, date_exposed, name = "new_infections") %>%
    group_by(loc_exposed) %>%
    mutate("cum_inf"  = cumsum(new_infections),
           "scenario" = scenario,
           "run"      = run) %>%
    ungroup()
  
  # infections per inc
  inf_per_inc <- exposure_info %>%
    bind_rows() %>%
    select(inc_exposed) %>%
    rename("inc_id" = inc_exposed) %>%
    mutate("scenario" = scenario,
           "run"      = run) %>%
    count(scenario, run, inc_id, name = "total_infections")
  
  
  # output
  list("cum_inf"=cum_inf, "inf_per_inc"=inf_per_inc)
}

inf_reassign_fun <- function(df) {
  inf_res_split <- df %>%
    filter(state %in% c("E", "I", "A")) %>%
    group_split(res_id)
  
  # exposure information by agent
  inf_res_split %>%
    map_dfr(function(res_df) {
      role <- ifelse("O-100" %in% res_df[["mod_id"]], "Overhead", "Crew")
      inf_inc <- res_df[["inc_id"]][1]
      inf_loc <- ifelse(inf_inc > 0, "On Fire", "Off Fire")
      days_missed <- length(which(res_df[["res_id"]][res_df[["quarantine"]]] > 0))
      non_q_df <- res_df %>% filter(!quarantine)
      
      inf_assign <- non_q_df[["inc_id"]][-1]
      inf_reassign <- length(unique(inf_assign[inf_assign > 0]))
      
      list("res_id"  = res_df[["res_id"]][1],
           "role"    = role,
           "inf_loc" = inf_loc,
           "inf_inc" = inf_inc,
           "inf_reassign" = inf_reassign,
           "days_missed"  = days_missed)
    }) %>%
    mutate("scenario" = df[["scenario"]][1],
           "run"      = df[["run"]][1])
}

# NOTE: if there are more than 2 overhead agents sick, that is an outbreak
#       if there are more than 2 modules with sick agents, that is an outbreak
is.outbreak <- function(inc_df) {
  n_inf_overhead <- length(unique(inc_df$res_id[inc_df$mod_id == "O-100" & !inc_df$quarantine]))
  n_inf_crews <- length(unique(inc_df$mod_id[!inc_df$quarantine]))
  ifelse((n_inf_overhead > 2L) | (n_inf_crews > 1L), TRUE, FALSE)
}

nested_outbreaks <- function(df) {
  df %>%
    filter(state %in% c("E", "I", "A") & inc_id > 0) %>%
    group_by(run, inc_id, scenario) %>%
    nest() %>%
    mutate("outbreak" = map_lgl(data, is.outbreak))
}

summarise_run <- function(file) {
  raw_dat <- read_csv(file)
  wd_missed <- wd_missed(raw_dat)
  inf_summaries <- summarise_inf(raw_dat)
  agent_reassignments <- inf_reassign_fun(raw_dat)
  outbreaks <- nested_outbreaks(raw_dat)
  
  list("wd_missed" = wd_missed,
       "cum_inf"   = inf_summaries$cum_inf,
       "inf_per_inc" = inf_summaries$inf_per_inc,
       "agent_reassignments" = agent_reassignments,
       "nested_outbreaks" = outbreaks)
}

get_metric <- function(metric, df) {
  out <- map_dfr(df, metric)
  if ("scenario" %in% colnames(out)) {
    out %>%
      mutate("scenario" = factor(scenario,
                                 levels = c("low", "high", "baseline"),
                                 labels = c("Low Compliance", "High Compliance", "Baseline")))
  }
  return(out)
} 

extract_metrics <- function(results) {
  result_labels <- c("wd_missed", "cum_inf", "inf_per_inc", "agent_resassignments", "nested_outbreaks")
  out <- map(result_labels, get_metric, df = results)
  names(out) <- result_names
  
  return(out)
}
