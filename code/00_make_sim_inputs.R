# Making complete input data for 2016, 2017, and 2018 seasons
#
# This script really only needs to be run once, unless a change needs to be
# made to the overheads ids, or vaccination plans

library(covidfireMASS)
library(readr)
library(dplyr)
library(purrr)

# quality of life functions
# why did I make vax plans so difficult?
simple_vax <- function(role_table, dates)
{
  base <- plan_vaccines(role_table, x = dates, y = c(0.5, 0.5, 0.5))
  base$plan["overhead"] <- round(approx(x = dates,
                                        y = c(0.5, 0.75, 0.75)*role_table["overhead"],
                                        n = dates[3])$y)
  
  low <- plan_vaccines(role_table, x = dates, y = c(0.5, 0.5, 0.5))
  
  high <- plan_vaccines(role_table, x = dates, y = c(0.5, 0.75, 0.75))
  high$plan["overhead"] <- round(approx(x = dates,
                                        y = c(0.7, 0.9, 0.9)*role_table["overhead"],
                                        n = dates[3])$y)
  
  list("base" = base, "low" = low, "high" = high)
}

#### 2016 ####
inc_id_2016 <- read_csv("inputs/originals/fire_camp_ABM_daily_res_assignments_inc_id_2016.csv")
mod_id_2016 <- read_csv("inputs/originals/fire_camp_ABM_daily_res_assignments_mod_id_2016.csv")
inc_info_2016 <- read_csv("inputs/originals/fire_camp_ABM_inc_info_2016.csv")

# Fix x's in original data set
inc_id_2016[inc_id_2016 == "X"] <- "0"
mod_id_2016[mod_id_2016 == "X"] <- "0"
inc_id_2016 <- type_convert(inc_id_2016) # X's made this load in as "char"
                                         # instead of numeric

# Fix missing home GACC's
all.equal(inc_id_2016$res_gacc, mod_id_2016$res_gacc) # double check things are lined up
new_gaccs_2016 <- clean_gacc(inc_id_2016, inc_info_2016)
inc_id_2016$res_gacc <- mod_id_2016$res_gacc <- new_gaccs_2016

# Make overhead_ids vector and write vaccination plans
july2016 <- which(colnames(inc_id_2016) == "2016-07-01")
tend2016 <- ncol(inc_id_2016) - 2
agent_roles_2016 <- find_role_type(mod_id_2016)
overhead_ids_2016 <- mod_id_2016$res_id[grepl("Overhead", agent_roles_2016)]
role_table_2016 <- table(ifelse(mod_id_2016$res_id %in% overhead_ids_2016, "overhead", "ground"))

plans_2016 <- simple_vax(role_table_2016, c(1, july2016, tend2016))
vax_plan_base_2016 <- plans_2016$base
vax_plan_low_2016  <- plans_2016$low
vax_plan_high_2016 <- plans_2016$high

save(list = c("inc_id_2016", "mod_id_2016", "inc_info_2016", "overhead_ids_2016",
              "vax_plan_base_2016", "vax_plan_low_2016", "vax_plan_high_2016"),
     file = "inputs/sim_inputs_2016_complete.rda")

#### 2017 ####
inc_id_2017 <- read_csv("inputs/originals/fire_camp_ABM_daily_res_assignments_inc_id_2017.csv")
mod_id_2017 <- read_csv("inputs/originals/fire_camp_ABM_daily_res_assignments_mod_id_2017.csv")
inc_info_2017 <- read_csv("inputs/originals/fire_camp_ABM_inc_info_2017.csv")

# Fix x's in original data set
inc_id_2017[inc_id_2017 == "X"] <- "0"
mod_id_2017[mod_id_2017 == "X"] <- "0"
inc_id_2017 <- type_convert(inc_id_2017)

# Fix missing home GACC's
all.equal(inc_id_2017$res_gacc, mod_id_2017$res_gacc)
new_gaccs_2017 <- clean_gacc(inc_id_2017, inc_info_2017)
inc_id_2017$res_gacc <- mod_id_2017$res_gacc <- new_gaccs_2017

# Make overhead_ids vector and write vaccination plans
july2017 <- which(colnames(inc_id_2017) == "2017-07-01")
tend2017 <- ncol(inc_id_2017) - 2
agent_roles_2017 <- find_role_type(mod_id_2017)
overhead_ids_2017 <- mod_id_2017$res_id[grepl("Overhead", agent_roles_2017)]
role_table_2017 <- table(ifelse(mod_id_2017$res_id %in% overhead_ids_2017, "overhead", "ground"))

plans_2017 <- simple_vax(role_table_2017, c(1, july2017, tend2017))
vax_plan_base_2017 <- plans_2017$base
vax_plan_low_2017  <- plans_2017$low
vax_plan_high_2017 <- plans_2017$high


save(list = c("inc_id_2017", "mod_id_2017", "inc_info_2017", "overhead_ids_2017",
              "vax_plan_base_2017", "vax_plan_low_2017", "vax_plan_high_2017"),
     file = "inputs/sim_inputs_2017_complete.rda")

#### 2018 ####
inc_id_2018 <- read_csv("inputs/originals/fire_camp_ABM_daily_res_assignments_inc_id_2018.csv")
mod_id_2018 <- read_csv("inputs/originals/fire_camp_ABM_daily_res_assignments_mod_id_2018.csv")
inc_info_2018 <- read_csv("inputs/originals/fire_camp_ABM_inc_info_2018.csv")

# Fix x's in original data set
inc_id_2018[inc_id_2018 == "X"] <- "0"
mod_id_2018[mod_id_2018 == "X"] <- "0"
inc_id_2018 <- type_convert(inc_id_2018)

# Fix missing home GACC's
all.equal(inc_id_2018$res_gacc, mod_id_2018$res_gacc)
new_gaccs_2018 <- clean_gacc(inc_id_2018, inc_info_2018)
inc_id_2018$res_gacc <- mod_id_2018$res_gacc <- new_gaccs_2018

# Make overhead_ids vector and write vaccination plans
july2018 <- which(colnames(inc_id_2018) == "2018-07-01")
tend2018 <- ncol(inc_id_2018) - 2
agent_roles_2018 <- find_role_type(mod_id_2018)
overhead_ids_2018 <- mod_id_2018$res_id[grepl("Overhead", agent_roles_2018)]
role_table_2018 <- table(ifelse(mod_id_2018$res_id %in% overhead_ids_2018, "overhead", "ground"))

plans_2018 <- simple_vax(role_table_2018, c(1, july2018, tend2018))
vax_plan_base_2018 <- plans_2018$base
vax_plan_low_2018  <- plans_2018$low
vax_plan_high_2018 <- plans_2018$high


save(list = c("inc_id_2018", "mod_id_2018", "inc_info_2018", "overhead_ids_2018",
              "vax_plan_base_2018", "vax_plan_low_2018", "vax_plan_high_2018"),
     file = "inputs/sim_inputs_2018_complete.rda")
