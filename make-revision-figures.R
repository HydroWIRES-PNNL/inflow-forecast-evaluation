# %% Packages and settings -----------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))

data_dir = "processed-data"
figure_dir = "paper/figures"
locations = c("wilder", "bellows", "vernon")
location_labels = c(
  wilder = "Wilder",
  bellows = "Bellows Falls",
  vernon = "Vernon"
)
source_levels = c("A", "B", "GRH")
source_colors = c(A = "#8C510A", B = "#01665E", GRH = "#5AB4AC")
threshold_cfs_hours = 15000
cfs_hours_per_foot = 30000

# %% Load hourly forecasts -----------------------------------------------------

forecasts = read_csv(
  file.path(data_dir, "forecasts_24h.csv"),
  show_col_types = FALSE,
  progress = FALSE
)

targets = read_csv(
  file.path(data_dir, "targets_24h.csv"),
  show_col_types = FALSE,
  progress = FALSE
) |>
  mutate(source = "grh") |>
  rename_with(\(x) str_replace(x, "_tgt$", "_fc"), ends_with("_tgt"))

hourly = bind_rows(forecasts, targets) |>
  mutate(
    source = recode(source, grh = "GRH"),
    source = factor(source, levels = source_levels)
  ) |>
  filter(
    !is.na(source),
    issue_time >= as.POSIXct("2023-06-01 12:00:00", tz = "US/Eastern"),
    issue_time < as.POSIXct("2025-05-01", tz = "US/Eastern")
  ) |>
  pivot_longer(
    cols = matches("_(fc|obs|obsprev)$"),
    names_to = c("location", ".value"),
    names_pattern = "(.*)_(fc|obs|obsprev)$"
  ) |>
  mutate(
    lead_time = as.numeric(difftime(valid_time, issue_time, units = "hours")),
    error = fc - obs
  ) |>
  filter(
    location %in% locations,
    lead_time > 0,
    if_all(c(fc, obs, obsprev), \(x) !is.na(x))
  ) |>
  arrange(location, source, issue_time, lead_time) |>
  group_by(location, source, issue_time) |>
  mutate(cum_error = cumsum(error)) |>
  ungroup() |>
  mutate(
    location = factor(location, levels = locations, labels = location_labels[locations]),
    cum_error_ft = cum_error / cfs_hours_per_foot
  )

summary_by_lead = hourly |>
  group_by(location, source, lead_time) |>
  summarise(
    q025 = quantile(cum_error_ft, 0.025),
    median = median(cum_error_ft),
    q975 = quantile(cum_error_ft, 0.975),
    p_exceed = mean(abs(cum_error) > threshold_cfs_hours),
    .groups = "drop"
  )

plot_summary = summary_by_lead |>
  filter(lead_time <= 72)

# %% Figure 5: cumulative error ------------------------------------------------

figure_5 = ggplot(
  plot_summary,
  aes(x = lead_time / 24, color = source, fill = source)
) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = "grey35") +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.13, color = NA) +
  geom_line(aes(y = median), linewidth = 0.8) +
  facet_wrap(vars(location), nrow = 1) +
  scale_color_manual(values = source_colors, drop = FALSE) +
  scale_fill_manual(values = source_colors, drop = FALSE) +
  scale_x_continuous(breaks = 0:3, limits = c(0, 3), expand = expansion(mult = c(0, 0.01))) +
  labs(
    x = "Lead time (days)",
    y = "Cumulative forebay error (ft)",
    color = "Forecast",
    fill = "Forecast"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#EEE8D5", color = "grey50")
  )

ggsave(
  file.path(figure_dir, "5-cum-error-5pct.png"),
  figure_5,
  width = 10,
  height = 4.6,
  dpi = 300,
  bg = "white"
)

# %% Figure 6: threshold probability ------------------------------------------

probability_summary = summary_by_lead |>
  filter(lead_time %in% c(24, 48, 72))

figure_6 = ggplot(
  probability_summary,
  aes(x = lead_time / 24, y = p_exceed, color = source, group = source)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  facet_wrap(vars(location), nrow = 1) +
  scale_color_manual(values = source_colors, drop = FALSE) +
  scale_x_continuous(breaks = 1:3, limits = c(1, 3)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(
    x = "Lead time (days)",
    y = "Probability outside forebay band",
    color = "Forecast"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#EEE8D5", color = "grey50")
  )

ggsave(
  file.path(figure_dir, "6a-threshold-1day.png"),
  figure_6,
  width = 10,
  height = 4.6,
  dpi = 300,
  bg = "white"
)

# %% Report values used in the manuscript -------------------------------------

summary_by_lead |>
  filter(lead_time %in% c(24, 48, 72)) |>
  mutate(p_exceed = round(100 * p_exceed, 1), day = lead_time / 24) |>
  select(location, source, day, p_exceed) |>
  arrange(location, day, source) |>
  print(n = Inf)
