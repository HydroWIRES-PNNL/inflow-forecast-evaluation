# %% ---------------------------------------------------------------------------
# Regenerate figures 3 and 4 of the paper from the published data.
#
#    3a-3d  daily forecast performance against lead time (KGE, NSE, PBIAS, r)
#    4a-4d  day-ahead performance by month           (KGE, NSE, PBIAS, r)
#
# These figures were previously exported by hand from the evaluation.Rmd
# dashboard, which is why they carried the dashboard's palette rather than the
# manuscript's. The plotting code here mirrors stat_plot_over_time() and
# stat_by_month() in evaluation.Rmd; the metric values and axis limits are
# unchanged. Only the styling comes from figure-style.R.
#
# The data pipeline is the same one verify-all.R uses, including the `perfect`
# and `persistence` benchmarks that are derived here rather than supplied in the
# data. The two commercial products are anonymized as A and B.
# %% ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(hydroGOF)
})

source("figure-style.R")

figure_dir <- "paper/figures"
all_locs <- c("wilder", "bellows", "vernon", "ottauquechee", "sugar", "white")

# %% Load and prepare ----------------------------------------------------------
# Mirrors verify-all.R.

load_window <- function(w) {
  fc <- read_csv(
    paste0("processed-data/forecasts_", w, ".csv"),
    show = F,
    progress = F
  )
  tg <- read_csv(
    paste0("processed-data/targets_", w, ".csv"),
    show = F,
    progress = F
  ) |>
    add_column(source = "grh") |>
    rename_with(\(x) str_replace(x, "_tgt$", "_fc"), ends_with("_tgt"))
  bind_rows(fc, tg)
}

label_sources <- function(x) {
  now <- now()
  x |>
    mutate(
      source = case_when(
        source == "A" ~ "A",
        source == "B" ~ "B",
        source == "grh" ~ "GRH",
        .default = NA
      )
    ) |>
    filter(
      issue_time >= ISOdate(2023, 6, 1, tz = "US/Eastern"),
      issue_time <=
        ISOdate(
          year(now),
          month(floor_date(now, "month")),
          1,
          tz = "US/Eastern"
        )
    ) |>
    mutate(
      season = case_when(
        month(issue_time) %in% c(12, 1, 2) ~ "winter",
        month(issue_time) %in% 3:5 ~ "spring",
        month(issue_time) %in% 6:8 ~ "summer",
        month(issue_time) %in% 9:11 ~ "fall"
      )
    )
}

build_daily <- function(forecasts) {
  forecasts |>
    distinct(valid_time, source, .keep_all = T) |>
    select(-issue_time, -season) |>
    pivot_longer(
      -c(valid_time, source),
      names_sep = "_",
      names_to = c("location", "type")
    ) |>
    mutate(valid_date = floor_date(valid_time, "day")) |>
    select(-valid_time) |>
    group_by(valid_date, location, type) |>
    mutate(location = factor(location, levels = all_locs)) |>
    group_by(valid_date, source, location, type) |>
    summarise(value = mean(value), .groups = "drop") |>
    pivot_wider(id_cols = c(valid_date, source, location), names_from = type) |>
    mutate(obsprev = lag(obs)) |>
    select(-fc, -obs) |>
    na.omit() -> obs_prev_day

  forecasts |>
    group_by(issue_time) |>
    mutate(valid_date = floor_date(valid_time, "day")) |>
    pivot_longer(
      -c(valid_time, issue_time, source, valid_date, season),
      names_sep = "_",
      names_to = c("location", "type")
    ) |>
    mutate(location = factor(location, levels = all_locs)) |>
    group_by(valid_date, issue_time, source, location, type, season) |>
    summarise(value = mean(value), .groups = "drop") |>
    mutate(
      lead_time = difftime(valid_date, issue_time, units = "hours") |>
        as.numeric()
    ) |>
    left_join(obs_prev_day, by = join_by(valid_date, source, location)) |>
    mutate(value = ifelse(type == "obsprev", obsprev, value)) |>
    select(-obsprev) |>
    pivot_wider(
      id_cols = c(valid_date, issue_time, source, location, lead_time, season),
      names_from = type
    ) |>
    na.omit() |>
    mutate(error = fc - obs) %>%
    bind_rows(
      . |>
        filter(source == "B") |>
        mutate(source = "perfect", fc = obs, error = fc - obs)
    ) %>%
    bind_rows(
      . |>
        filter(source == "B") |>
        group_by(location, issue_time) |>
        mutate(source = "persistence", fc = obsprev[1], error = fc - obs)
    ) |>
    filter(location %in% locations)
}

fc_stats <- function(x) {
  x |>
    group_by(source, location, lead_time) |>
    mutate(n = n()) |>
    filter(n > 1) |>
    summarise(
      NSE = NSE(fc, obs),
      KGE = KGE(fc, obs),
      PBIAS = pbias(fc, obs),
      r = rPearson(fc, obs),
      .groups = "drop"
    )
}

fc_stats_by_month <- function(x) {
  x |>
    mutate(month = month(issue_time)) |>
    group_by(source, location, lead_time, month) |>
    mutate(n = n()) |>
    filter(n > 1) |>
    summarise(
      NSE = NSE(fc, obs),
      KGE = KGE(fc, obs),
      PBIAS = pbias(fc, obs),
      r = rPearson(fc, obs),
      .groups = "drop"
    )
}

daily <- load_window("24h") |>
  label_sources() |>
  build_daily()

# GRH is issued a day earlier than the commercial products, so its lead time is
# offset by 23 h before the day-ahead window is taken. Same as evaluation.Rmd.
day1 <- daily |>
  mutate(lead_time = ifelse(source == "GRH", lead_time + 23, lead_time)) |>
  filter(lead_time <= 24, lead_time > 0)

stats_daily <- daily |>
  fc_stats() |>
  mutate(
    location = label_locations(location),
    source = factor(source, levels = source_levels)
  )

stats_da_by_month <- day1 |>
  fc_stats_by_month() |>
  filter(lead_time >= 20) |>
  mutate(
    location = label_locations(location),
    source = factor(source, levels = source_levels)
  )

# %% Figure 3: daily performance against lead time -----------------------------

stat_over_lead <- function(x, stat, ylim) {
  ggplot(x) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey35") +
    geom_line(
      aes(lead_time / 24, !!as.name(stat), color = source, linetype = source),
      linewidth = 0.8
    ) +
    scale_x_continuous(breaks = 0:10, minor_breaks = NULL) +
    facet_wrap(vars(location), nrow = 1) +
    scale_source() +
    coord_cartesian(ylim = ylim) +
    theme_paper() +
    labs(x = "Lead time (days)", y = stat)
}

figure_3 <- list(
  "3a-daily-kge.png" = stat_over_lead(stats_daily, "KGE", c(-0.41, 1)),
  "3b-daily-nse.png" = stat_over_lead(stats_daily, "NSE", c(-1, 1)),
  "3c-daily-pbias.png" = stat_over_lead(stats_daily, "PBIAS", c(-75, 50)),
  "3d-daily-r.png" = stat_over_lead(stats_daily, "r", c(0, 1))
)

# %% Figure 4: day-ahead performance by month ----------------------------------

stat_by_month <- function(x, stat, ylim) {
  x |>
    mutate(plot_month = ISOdate(ifelse(month >= 6, 2000, 2001), month, 1)) |>
    ggplot() +
    # The no-skill threshold differs by metric: using the mean of the
    # observations as a predictor gives NSE = 0 but KGE = 1 - sqrt(2) ~ -0.41
    # (Knoben et al., 2019), so the reference line must be metric-specific.
    geom_hline(
      yintercept = ifelse(stat == "KGE", -0.41, 0),
      linetype = "dashed",
      color = "grey35"
    ) +
    geom_line(
      aes(plot_month, !!as.name(stat), color = source, linetype = source),
      linewidth = 0.8
    ) +
    facet_wrap(vars(location), nrow = 1) +
    scale_x_datetime(
      date_breaks = "1 month",
      date_labels = "%b",
      minor_breaks = NULL
    ) +
    scale_source() +
    coord_cartesian(ylim = ylim) +
    theme_paper() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    labs(x = "", y = stat)
}

figure_4 <- list(
  "4a-da-month-kge.png" = stat_by_month(stats_da_by_month, "KGE", c(-0.41, 1)),
  "4b-da-month-nse.png" = stat_by_month(stats_da_by_month, "NSE", c(-1, 1)),
  "4c-da-month-pbias.png" = stat_by_month(
    stats_da_by_month,
    "PBIAS",
    c(-100, 100)
  ),
  "4d-da-month-r.png" = stat_by_month(stats_da_by_month, "r", c(0, 1))
)

# %% Write ---------------------------------------------------------------------

for (nm in names(figure_3)) {
  ggsave(
    file.path(figure_dir, nm),
    figure_3[[nm]],
    width = 10,
    height = 3.2,
    dpi = 300,
    bg = "white"
  )
}

for (nm in names(figure_4)) {
  ggsave(
    file.path(figure_dir, nm),
    figure_4[[nm]],
    width = 10,
    height = 3.6,
    dpi = 300,
    bg = "white"
  )
}

cat("Wrote", length(figure_3) + length(figure_4), "figures to", figure_dir, "\n")

# Series present in each figure, so a missing line is visibly a data gap rather
# than a silent drop.
cat("\n--- series coverage, figure 3 (daily vs lead time) ---\n")
stats_daily |>
  group_by(source) |>
  summarise(
    n_points = n(),
    max_lead_days = round(max(lead_time) / 24, 1),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

cat("\n--- series coverage, figure 4 (day-ahead by month) ---\n")
stats_da_by_month |>
  group_by(source) |>
  summarise(n_months = n_distinct(month), .groups = "drop") |>
  as.data.frame() |>
  print()
