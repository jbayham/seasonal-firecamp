#### Libraries ####
library(dplyr)
library(readr)
library(ggplot2)
library(ggridges)
library(purrr)

#### Worker Days Missed ####

# Plots
wdm_2017 <- read_csv("outputs/tables/2017_all_scenarios_worker_days_missed.csv")
wdm_plot_dat <- make_plot_dat(wdm_2017)
plot_wd_missed(wdm_plot_dat)
ggsave("outputs/figs/2017_all_scenarios_worker_days_missed_ridges.png",
       height = 2.5, width = 7, units = "in")

#### Cumulative Infections ####


# Plots
cum_inf_2017 <- read_csv("outputs/tables/2017_all_scenarios_cumulative_infections.csv")

cum_inf_2017 %>%
  filter(!(scenario=="No Policy")) %>%
  plot_cum_inf()

ggsave("outputs/figs/2017_all_scenarios_cumulative_infections.png",
       height = 2.5, width = 7, units = "in")
