#### Libraries ####
library(dplyr)
library(readr)
library(ggplot2)
library(ggridges)
library(purrr)

#### Worker Days Missed ####
wdm_iqr <- function(df) {
  df %>%
    group_by(scenario, vac_include) %>%
    summarise(across(wd_missed, list("y_med" = median,
                                     "y_lower" = ~quantile(., 0.25),
                                     "y_upper" = ~quantile(., 0.75)),
                     .names = "{.fn}"))
}
make_plot_dat <- function(df) {
  bind_rows(
    df %>%
      mutate("vac_include" = "All"),
    df %>%
      filter(!vaccinated) %>%
      mutate("vac_include" = "Only Unvaccinated")
  )
}
my_pal_duo <- paste0("#", c("F71735",
                           "8783D1"))
plot_wd_missed <- function(plot_dat) {
  iqr <- wdm_iqr(plot_dat)
  plot_dat %>%
    ggplot(aes(y = scenario, x = wd_missed, fill = vac_include, color = vac_include)) +
    geom_density_ridges(jittered_points = TRUE, scale = 0.95, rel_min_height = 0.1,
                        point_shape = "|", point_size = 3, size = 0.25, alpha = 0.5,
                        position = position_points_jitter(height = 0), show.legend = TRUE) +
    geom_text(data = iqr, aes(y = scenario, x = y_med),
              label = "+", alpha = 1, size = 4, nudge_y = -0.15,
              show.legend = FALSE) +
    geom_text(data = iqr, aes(y = scenario, x = y_lower),
              label="[", alpha = 1, size = 3, nudge_y = -0.15,
              show.legend = FALSE) +
    geom_text(data = iqr, aes(y = scenario, x = y_upper),
              label = "]", alpha = 1, size = 3, nudge_y = -0.15,
              show.legend = FALSE) +
    scale_fill_manual(name="",values = my_pal_duo) +
    scale_color_manual(name="",values = my_pal_duo) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(# title = "Scenarios for 2017 Fire Season",
         # subtitle = "Worker Days Missed",
         x = "", y = "") +
    xlim(c(0, 4500))
}

# Plots
wdm_2017 <- read_csv("outputs/tables/2017_all_scenarios_worker_days_missed.csv")
wdm_plot_dat <- make_plot_dat(wdm_2017)
plot_wd_missed(wdm_plot_dat)
ggsave("outputs/figs/2017_all_scenarios_worker_days_missed_ridges.png",
       height = 2.5, width = 7, units = "in")

#### Cumulative Infections ####
plot_cum_inf <- function(plot_dat) {
  plot_dat %>%
    ggplot(aes(x = date_exposed, y = cum_inf, color = scenario)) +
    geom_line(aes(group = run), alpha = 0.5, size = 0.5, show.legend = TRUE) +
    geom_smooth(aes(group = scenario), se = FALSE, color = "black", size = 0.8) +
    facet_grid(rows = vars(loc_exposed), cols = vars(scenario), scales = "free") +
    theme_bw() +
    labs(x = "Time (days)",
         y = "Cumulative Infections")
}

# Plots
cum_inf_2017 <- ead_csv("outpubs/tables/2017_all_scenarios_cumulative_infections.csv")
plot_cum_inf(cum_inf_2017)
ggsave("outputs/figs/2017_all_scenarios_cumulative_infections.png",
       height = 2.5, width = 7, units = "in")
