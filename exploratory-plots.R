library(tidyverse)

forecasts_0h <- read_csv("processed-data/forecasts_00h.csv", show = F, progress = F)
forecasts_6h <- read_csv("processed-data/forecasts_06h.csv", show = F, progress = F)
forecasts_12h <- read_csv("processed-data/forecasts_12h.csv", show = F, progress = F)
forecasts_24h <- read_csv("processed-data/forecasts_24h.csv", show = F, progress = F)

fc0_long <- forecasts_0h |>
  mutate(issue_time_source = paste0(issue_time, source)) |>
  pivot_longer(-c(valid_time, issue_time, issue_time_source, source))

plot_all_forecasts <- function(x, dam, prev_hours = 24) {
  dam_fc <- paste0(dam, "_fc")
  dam_obs <- paste0(dam, "_obs")
  dam_name <- str_to_title(dam)
  colors <- c("Obs" = "black", "Fc" = "grey")
  x |>
    mutate(issue_time_source = paste0(issue_time, source)) |>
    ggplot() +
    geom_line(aes(valid_time, !!as.name(dam_fc), group = issue_time_source, color = "Fc")) +
    geom_line(aes(valid_time, !!as.name(dam_obs), color = "Obs")) +
    scale_color_manual("", values = colors) +
    theme_bw() +
    labs(y = sprintf("%s Total Inflow [cfs]", dam_name), x = "") ->
  p
  ggsave(sprintf("plots/exploratory/%s_%sh.png", dam, prev_hours), p, width = 11, height = 3)
}

plot_all_forecasts(forecasts_0h, "wilder", 0)
plot_all_forecasts(forecasts_24h, "wilder", 24)

plot_all_forecasts(forecasts_0h, "bellows", 0)
plot_all_forecasts(forecasts_24h, "bellows", 24)

plot_all_forecasts(forecasts_0h, "vernon", 0)
plot_all_forecasts(forecasts_24h, "vernon", 24)

plot_all_forecasts(forecasts_0h, "sugar", 0)
plot_all_forecasts(forecasts_0h, "ottauquechee", 0)
plot_all_forecasts(forecasts_0h, "white", 0)
