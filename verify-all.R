# %% ---------------------------------------------------------------------------
# Reproduces the quantitative results reported in Bracken et al., "A real-time
# reservoir inflow forecast evaluation framework."
#
# Usage, from the repository root, after downloading the dataset from Zenodo
# (https://doi.org/10.5281/zenodo.16921728) into ./processed-data/:
#
#   R --no-save --no-restore -f verify-all.R
#
# Sections:
#    1  Day-ahead evaluation metrics (Table 1 of the paper)
#    2  Skill score relative to the persistence benchmark
#    3  Evaluation period extent and per-product coverage
#    4  Metrics restricted to the period common to all three products
#    5  Sensitivity of the metrics to the smoothing window width
#    6  Seasonal ordering of forecast performance
#    7  Largest inflow events during the evaluation period
#    8  Issue frequency and forecast horizon of each product
#    9  Percent bias by lead time and location
#   10  Forebay threshold exceedance probability
#
# Note on section 10: the exceedance calculation as published in evaluation.Rmd
# groups by valid_time, so cumsum() operates on groups of size one and the
# quantity obtained is the instantaneous hourly error rather than an
# accumulation over lead time. This script reports that quantity alongside a
# genuinely cumulative version so the difference between the two is explicit.
#
# The two commercial forecast products are anonymized as A and B, consistent
# with the paper and the published dataset.
# %% ---------------------------------------------------------------------------

all_locs <- c("wilder", "bellows", "vernon", "ottauquechee", "sugar", "white")
locs <- c("wilder", "bellows", "vernon")

suppressPackageStartupMessages(library(tidyverse))
suppressMessages(import::from(
  hydroGOF,
  NSE,
  KGE,
  pbias,
  nrmse,
  rPearson,
  rSD,
  rmse
))

# %% Load and prepare -----------------------------------------------------------

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
    rename(
      wilder_fc = wilder_tgt,
      bellows_fc = bellows_tgt,
      vernon_fc = vernon_tgt,
      white_fc = white_tgt,
      ottauquechee_fc = ottauquechee_tgt,
      sugar_fc = sugar_tgt
    )
  out <- bind_rows(fc, tg)

  # The 00h files are built from unsmoothed `grh`, which format-data.R does not
  # pass through lag_variables(), so *_obsprev is absent. Rebuild it the same way.
  if (!"wilder_obsprev" %in% names(out)) {
    prev <- out |>
      distinct(valid_time, .keep_all = TRUE) |>
      arrange(valid_time) |>
      select(valid_time, ends_with("_obs")) |>
      mutate(across(ends_with("_obs"), lag)) |>
      rename_with(\(x) str_replace(x, "_obs$", "_obsprev"), ends_with("_obs"))
    out <- out |> left_join(prev, by = "valid_time")
  }
  out
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
    filter(location %in% locs)
}

# Exact Table 1 path: offset GRH by 23 h, take day 1, group by lead_time, keep >= 20.
day_ahead_stats <- function(daily) {
  daily |>
    mutate(lead_time = ifelse(source == "GRH", lead_time + 23, lead_time)) |>
    filter(lead_time <= 24, lead_time > 0) |>
    group_by(source, location, lead_time) |>
    mutate(nn = n()) |>
    filter(nn > 1) |>
    summarise(
      n = n(),
      NSE = NSE(fc, obs),
      KGE = KGE(fc, obs),
      PBIAS = pbias(fc, obs),
      RMSE = rmse(fc, obs),
      nRMSE = nrmse(fc, obs),
      r = rPearson(fc, obs),
      rSD = rSD(fc, obs),
      mse = mean((fc - obs)^2),
      .groups = "drop"
    ) |>
    filter(lead_time >= 20)
}

raw24 <- load_window("24h")
fc24 <- raw24 |> label_sources()
daily24 <- fc24 |> build_daily()
da24 <- daily24 |>
  mutate(lead_time = ifelse(source == "GRH", lead_time + 23, lead_time)) |>
  filter(lead_time <= 24, lead_time > 0, lead_time >= 20)

# %% 1. Table 1 ----------------------------------------------------------------

cat("\n\n########## 1. TABLE 1 (compare to main.tex) ##########\n")
tab1 <- daily24 |> day_ahead_stats()
tab1 |>
  arrange(location, source) |>
  mutate(
    across(c(NSE, KGE, r, rSD), \(x) round(x, 2)),
    PBIAS = round(PBIAS, 1),
    RMSE = round(RMSE, 0),
    nRMSE = round(nRMSE, 1)
  ) |>
  select(source, location, n, NSE, KGE, PBIAS, RMSE, nRMSE, r, rSD) |>
  as.data.frame() |>
  print()

# %% 2. Persistence skill score ------------------------------------------------

cat("\n\n########## 2. SKILL SCORES VS THE PERSISTENCE BENCHMARK ##########\n")

# KGE skill score, as reported in Table 1 of the paper:
#   KGE_SS = (KGE_f - KGE_ref) / (1 - KGE_ref),  reference = persistence
# Computed from unrounded KGE so it matches the published column.
cat("\n--- KGE skill score (Table 1 column KGE_SS) ---\n")
tab1 |>
  select(location, source, KGE) |>
  group_by(location) |>
  mutate(kge_ref = KGE[source == "persistence"]) |>
  ungroup() |>
  filter(source %in% c("A", "B", "GRH")) |>
  mutate(ss = round((KGE - kge_ref) / (1 - kge_ref), 2)) |>
  select(location, source, ss) |>
  pivot_wider(names_from = source, values_from = ss) |>
  as.data.frame() |>
  print()

# The equivalent skill score built on mean squared error. Reported alongside
# because it weights large errors differently and gives a noticeably different
# picture at the most regulated site: at Vernon forecast A scores 0.03 here
# against 0.47 on the KGE version.
cat("\n--- MSE skill score, for comparison ---\n")
tab1 |>
  select(location, source, mse) |>
  group_by(location) |>
  mutate(mse_p = mse[source == "persistence"]) |>
  ungroup() |>
  filter(source %in% c("A", "B", "GRH")) |>
  mutate(ss = round(1 - mse / mse_p, 2)) |>
  select(location, source, ss) |>
  pivot_wider(names_from = source, values_from = ss) |>
  as.data.frame() |>
  print()

# %% 3. Evaluation period and per-product coverage -----------------------------

cat("\n\n########## 3. EVALUATION PERIOD ##########\n")
cat("--- observation record extent (smoothed calculated inflow) ---\n")
raw24 |>
  select(valid_time, ends_with("_obs")) |>
  distinct(valid_time, .keep_all = TRUE) |>
  pivot_longer(-valid_time, names_to = "location", values_to = "obs") |>
  na.omit() |>
  summarise(first = min(valid_time), last = max(valid_time)) |>
  as.data.frame() |>
  print()

cat("\n--- day-ahead sample coverage by product ---\n")
da24 |>
  group_by(source) |>
  summarise(
    first = min(valid_date),
    last = max(valid_date),
    n_days = n_distinct(valid_date),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

# %% 4. Common-period comparability -------------------------------------------

cat(
  "\n\n########## 4. COMMON-PERIOD METRICS (A, B, GRH all present) ##########\n"
)
common <- da24 |>
  filter(source %in% c("A", "B", "GRH")) |>
  distinct(source, location, valid_date) |>
  count(location, valid_date) |>
  filter(n == 3) |>
  select(location, valid_date)

cat("common days per location:", nrow(common) / 3, "\n")
cat(
  "span:",
  format(min(common$valid_date)),
  "to",
  format(max(common$valid_date)),
  "\n\n"
)

com <- da24 |>
  inner_join(common, by = c("location", "valid_date")) |>
  group_by(source, location) |>
  filter(n() > 1) |>
  summarise(
    n = n(),
    NSE = round(NSE(fc, obs), 2),
    KGE = round(KGE(fc, obs), 2),
    PBIAS = round(pbias(fc, obs), 1),
    RMSE = round(rmse(fc, obs), 0),
    r = round(rPearson(fc, obs), 2),
    mse = mean((fc - obs)^2),
    .groups = "drop"
  )
com |>
  arrange(location, source) |>
  select(-mse) |>
  as.data.frame() |>
  print()

cat("\n--- common-period skill score vs persistence ---\n")
com |>
  select(location, source, mse) |>
  group_by(location) |>
  mutate(mse_p = mse[source == "persistence"]) |>
  ungroup() |>
  filter(source %in% c("A", "B", "GRH")) |>
  mutate(ss = round(1 - mse / mse_p, 2)) |>
  select(location, source, ss) |>
  pivot_wider(names_from = source, values_from = ss) |>
  as.data.frame() |>
  print()

# %% 5. Smoothing sensitivity --------------------------------------------------

cat("\n\n########## 5. SMOOTHING SENSITIVITY ##########\n")
wlab <- c("00h" = "0 h", "06h" = "6 h", "12h" = "12 h", "24h" = "24 h")
sens <- map(names(wlab), \(w) {
  load_window(w) |>
    label_sources() |>
    build_daily() |>
    day_ahead_stats() |>
    mutate(window = wlab[w])
}) |>
  list_rbind()

cat("--- day-ahead NSE ---\n")
sens |>
  filter(source %in% c("A", "B", "GRH")) |>
  mutate(NSE = round(NSE, 2)) |>
  select(location, source, window, NSE) |>
  pivot_wider(names_from = window, values_from = NSE) |>
  arrange(location, source) |>
  as.data.frame() |>
  print()

cat("\n--- day-ahead KGE ---\n")
sens |>
  filter(source %in% c("A", "B", "GRH")) |>
  mutate(KGE = round(KGE, 2)) |>
  select(location, source, window, KGE) |>
  pivot_wider(names_from = window, values_from = KGE) |>
  arrange(location, source) |>
  as.data.frame() |>
  print()

cat("\n--- B minus A NSE gap (stability of the ranking) ---\n")
sens |>
  filter(source %in% c("A", "B")) |>
  select(location, source, window, NSE) |>
  pivot_wider(names_from = source, values_from = NSE) |>
  mutate(gap = round(B - A, 2)) |>
  select(location, window, gap) |>
  pivot_wider(names_from = window, values_from = gap) |>
  as.data.frame() |>
  print()

# %% 6. Seasonal ordering ------------------------------------------------------

cat(
  "\n\n########## 6. SEASONAL ORDERING (pooled across the three dams) ##########\n"
)
da24 |>
  filter(source %in% c("A", "B", "GRH")) |>
  group_by(source, season) |>
  summarise(
    KGE = round(KGE(fc, obs), 2),
    NSE = round(NSE(fc, obs), 2),
    n = n(),
    .groups = "drop"
  ) |>
  arrange(source, desc(KGE)) |>
  as.data.frame() |>
  print()

cat(
  "\n--- mean observed inflow by month (identifies the high-flow season) ---\n"
)
da24 |>
  filter(source == "B") |>
  mutate(month = month(valid_date)) |>
  group_by(location, month) |>
  summarise(mean_obs = round(mean(obs), 0), .groups = "drop") |>
  pivot_wider(names_from = location, values_from = mean_obs) |>
  arrange(month) |>
  as.data.frame() |>
  print()

# %% 7. Event peaks ------------------------------------------------------------

cat("\n\n########## 7. LARGEST EVENTS IN THE EVALUATION PERIOD ##########\n")
obs_all <- raw24 |>
  select(valid_time, ends_with("_obs")) |>
  distinct(valid_time, .keep_all = TRUE) |>
  pivot_longer(-valid_time, names_to = "location", values_to = "obs") |>
  mutate(location = str_remove(location, "_obs")) |>
  na.omit()

decluster <- function(d, gap_days = 5, n = 3) {
  d <- d |> arrange(desc(obs))
  keep <- tibble()
  for (i in seq_len(nrow(d))) {
    if (nrow(keep) >= n) {
      break
    }
    if (
      nrow(keep) == 0 ||
        all(
          abs(as.numeric(difftime(
            d$valid_time[i],
            keep$valid_time,
            units = "days"
          ))) >
            gap_days
        )
    ) {
      keep <- bind_rows(keep, d[i, ])
    }
  }
  keep
}

obs_all |>
  group_split(location) |>
  map(\(d) decluster(d) |> mutate(rank = row_number())) |>
  list_rbind() |>
  mutate(obs = round(obs, 0), date = as_date(valid_time)) |>
  select(location, rank, date, obs) |>
  as.data.frame() |>
  print()

# %% 8. Product metadata -------------------------------------------------------

cat("\n\n########## 8. PRODUCT ISSUE FREQUENCY AND HORIZON ##########\n")
bind_rows(
  read_csv("processed-data/forecasts_24h.csv", show = F, progress = F) |>
    select(valid_time, issue_time, source),
  read_csv("processed-data/targets_24h.csv", show = F, progress = F) |>
    select(valid_time, issue_time) |>
    add_column(source = "grh")
) |>
  mutate(
    source = case_when(
      source == "A" ~ "A",
      source == "B" ~ "B",
      source == "grh" ~ "GRH",
      .default = source
    )
  ) |>
  mutate(
    lead = as.numeric(difftime(valid_time, issue_time, units = "hours"))
  ) |>
  group_by(source) |>
  summarise(
    n_issues = n_distinct(issue_time),
    n_days = n_distinct(as_date(issue_time)),
    issues_per_day = round(
      n_distinct(issue_time) / n_distinct(as_date(issue_time)),
      2
    ),
    issue_hours = paste(sort(unique(hour(issue_time))), collapse = ", "),
    max_lead_h = max(lead),
    .groups = "drop"
  ) |>
  as.data.frame() |>
  print()

# %% 9. Bias by lead time (checks the "downstream propagation" claim) ----------

cat("\n\n########## 9. PBIAS BY LEAD TIME AND LOCATION ##########\n")
fc24 |>
  filter(source %in% c("A", "B")) |>
  mutate(valid_date = floor_date(valid_time, "day")) |>
  pivot_longer(
    -c(valid_time, issue_time, source, valid_date, season),
    names_sep = "_",
    names_to = c("location", "type")
  ) |>
  group_by(valid_date, issue_time, source, location, type) |>
  summarise(value = mean(value), .groups = "drop") |>
  mutate(
    lead_time = as.numeric(difftime(valid_date, issue_time, units = "hours"))
  ) |>
  pivot_wider(
    id_cols = c(valid_date, issue_time, source, location, lead_time),
    names_from = type
  ) |>
  na.omit() |>
  filter(location %in% locs) |>
  mutate(day = ceiling(lead_time / 24)) |>
  filter(day %in% 1:7) |>
  group_by(source, location, day) |>
  filter(n() > 2) |>
  summarise(PBIAS = round(pbias(fc, obs), 1), .groups = "drop") |>
  pivot_wider(names_from = location, values_from = PBIAS) |>
  arrange(source, day) |>
  as.data.frame() |>
  print()

# %% 10. Forebay exceedance ----------------------------------------------------
# NOTE: reproduces the evaluation.Rmd calculation, then contrasts it with a
# genuinely cumulative version. See the co-author note in the response.

cat("\n\n########## 10. FOREBAY EXCEEDANCE ##########\n")
threshold <- 15000

hourly <- fc24 |>
  group_by(issue_time) |>
  pivot_longer(
    -c(valid_time, issue_time, source, season),
    names_sep = "_",
    names_to = c("location", "type")
  ) |>
  mutate(
    location = factor(location, levels = all_locs),
    lead_time = as.numeric(difftime(valid_time, issue_time, units = "hours"))
  ) |>
  pivot_wider(
    id_cols = c(valid_time, issue_time, source, location, lead_time),
    names_from = type
  ) |>
  na.omit() |>
  mutate(error = fc - obs) |>
  filter(location %in% locs)

# As published: cumsum() over groups of size one, so this is the instantaneous
# hourly error, not an accumulation.
as_published <- hourly |>
  group_by(location, valid_time, issue_time, source, lead_time) |>
  summarise(cum_error = cumsum(error), .groups = "drop")

cat("group sizes in the published grouping (all 1 => cumsum is a no-op):\n")
hourly |>
  count(location, valid_time, issue_time, source, lead_time, name = "sz") |>
  count(sz) |>
  as.data.frame() |>
  print()

# Genuinely cumulative: integrate error over lead time within each issue.
truly_cumulative <- hourly |>
  arrange(location, source, issue_time, lead_time) |>
  group_by(location, source, issue_time) |>
  mutate(true_cum = cumsum(error)) |>
  ungroup()

cat(
  "\n--- P(exceed 0.5 ft), neutral start: as published vs truly cumulative ---\n"
)
truly_cumulative |>
  filter(source %in% c("A", "B"), lead_time %in% c(24, 48, 72)) |>
  group_by(location, source, lead_time) |>
  summarise(
    p_published = round(100 * mean(abs(error) > threshold), 1),
    p_cumulative = round(100 * mean(abs(true_cum) > threshold), 1),
    .groups = "drop"
  ) |>
  mutate(day = lead_time / 24) |>
  select(-lead_time) |>
  as.data.frame() |>
  print()

cat("\n--- P(exceed 0.5 ft), second day non-neutral start (as published) ---\n")
as_published |>
  group_by(location, issue_time, source) |>
  mutate(cum_error = cum_error + cum_error[25]) |>
  group_by(location, lead_time, source) |>
  summarise(p = mean(abs(cum_error) > threshold), .groups = "drop") |>
  filter(source %in% c("A", "B"), lead_time %in% c(24, 48, 72)) |>
  mutate(p = round(100 * p, 1), day = lead_time / 24) |>
  select(location, day, source, p) |>
  pivot_wider(names_from = source, values_from = p) |>
  arrange(location, day) |>
  as.data.frame() |>
  print()

cat("\nDone.\n")
