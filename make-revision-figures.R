# %% Packages and settings -----------------------------------------------------

suppressPackageStartupMessages(library(tidyverse))

data_dir <- "processed-data"
figure_dir <- "paper/figures"
locations <- c("wilder", "bellows", "vernon")
location_labels <- c(
  wilder = "Wilder",
  bellows = "Bellows Falls",
  vernon = "Vernon"
)
source_levels <- c("A", "B", "GRH")
source_colors <- c(A = "#8C510A", B = "#01665E", GRH = "#5AB4AC")
threshold_cfs_hours <- 15000
cfs_hours_per_foot <- 30000

# %% Load hourly forecasts -----------------------------------------------------

forecasts <- read_csv(
  file.path(data_dir, "forecasts_24h.csv"),
  show_col_types = FALSE,
  progress = FALSE
)

targets <- read_csv(
  file.path(data_dir, "targets_24h.csv"),
  show_col_types = FALSE,
  progress = FALSE
) |>
  mutate(source = "grh") |>
  rename_with(\(x) str_replace(x, "_tgt$", "_fc"), ends_with("_tgt"))

hourly <- bind_rows(forecasts, targets) |>
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
  # Accumulate from each issue's own first delivered hour rather than from a
  # fixed absolute lead. Forecast A is issued at 04:00 but for 1146 of its 1617
  # issues the delivered series begins at lead hour 25, so an absolute-lead axis
  # compares 8 accumulated hours of A against 32 of B and produces a
  # non-monotonic A curve driven purely by sample composition. Indexing by
  # position in the horizon keeps every row and makes each point a like-for-like
  # N-hour accumulation. A's horizon offset is reported below for the caption.
  mutate(
    horizon_hour = row_number(),
    cum_error = cumsum(error)
  ) |>
  ungroup() |>
  mutate(
    location = factor(
      location,
      levels = locations,
      labels = location_labels[locations]
    ),
    cum_error_ft = cum_error / cfs_hours_per_foot
  )

# GRH targets extend only 32 h past issue, so 32 h is the longest horizon over
# which all three products can be compared.
max_horizon_hour <- 32

summary_by_horizon <- hourly |>
  group_by(location, source, horizon_hour) |>
  summarise(
    n_issues = n(),
    mean_lead = mean(lead_time),
    q025 = quantile(cum_error_ft, 0.025),
    median = median(cum_error_ft),
    q975 = quantile(cum_error_ft, 0.975),
    p_exceed = mean(abs(cum_error) > threshold_cfs_hours),
    .groups = "drop"
  )

plot_summary <- summary_by_horizon |>
  filter(horizon_hour <= max_horizon_hour)

# %% Figure 5: cumulative error ------------------------------------------------

figure_5 <- ggplot(
  plot_summary,
  aes(x = horizon_hour, color = source, fill = source)
) +
  geom_hline(yintercept = c(-0.5, 0.5), linetype = "dashed", color = "grey35") +
  geom_ribbon(aes(ymin = q025, ymax = q975), alpha = 0.13, color = NA) +
  geom_line(aes(y = median), linewidth = 0.8) +
  facet_wrap(vars(location), nrow = 1) +
  scale_color_manual(values = source_colors, drop = FALSE) +
  scale_fill_manual(values = source_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = seq(8, max_horizon_hour, 8),
    limits = c(1, max_horizon_hour),
    expand = expansion(mult = 0.02)
  ) +
  labs(
    x = "Hours into forecast horizon",
    y = "Cumulative forebay error (ft)",
    color = "Forecast",
    fill = "Forecast"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(1, "lines"),
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

report_hours <- c(8, 16, 24, 32)

probability_summary <- summary_by_horizon |>
  filter(horizon_hour %in% report_hours)

figure_6 <- ggplot(
  probability_summary,
  aes(x = horizon_hour, y = p_exceed, color = source, group = source)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  facet_wrap(vars(location), nrow = 1) +
  scale_color_manual(values = source_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = report_hours,
    limits = range(report_hours),
    expand = expansion(mult = 0.04)
  ) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(
    x = "Hours into forecast horizon",
    y = "Probability outside forebay band",
    color = "Forecast"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(1, "lines"),
    strip.background = element_rect(fill = "#EEE8D5", color = "grey50")
  )

ggsave(
  file.path(figure_dir, "6-forebay-exceedance.png"),
  figure_6,
  width = 10,
  height = 4.6,
  dpi = 300,
  bg = "white"
)

# %% Report values used in the manuscript -------------------------------------

cat("\n--- P(outside 0.5 ft forebay band) by hours into horizon ---\n")
probability_summary |>
  mutate(p_exceed = round(100 * p_exceed, 1)) |>
  select(location, source, horizon_hour, n_issues, p_exceed) |>
  arrange(location, horizon_hour, source) |>
  print(n = Inf)

# Absolute lead times spanned by each product's first 32 forecast hours. Forecast
# A's horizon starts later than B's and GRH's, which the figure caption must
# state so the x-axis is not read as absolute lead time.
cat("\n--- absolute lead time spanned by each product's plotted horizon ---\n")
hourly |>
  filter(horizon_hour <= max_horizon_hour) |>
  group_by(source) |>
  summarise(
    first_hour_mean_lead = round(mean(lead_time[horizon_hour == 1]), 1),
    last_hour_mean_lead = round(
      mean(lead_time[horizon_hour == max_horizon_hour]),
      1
    ),
    n_issues = n_distinct(paste(location, issue_time)),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()
