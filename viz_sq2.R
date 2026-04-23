library(tidyverse)
library(sf)
library(tidycensus)

# Load data
df <- readRDS("data/clean_311_final.rds")
nyc_income <- readRDS("data/nyc_income_tracts.rds")
boot_ci <- readRDS("data/boot_ci.rds")

# ── 1. Choropleth: median resolution time by census tract ─────────────────────

tract_summary <- df |>
  filter(!is.na(geoid)) |>
  group_by(geoid) |>
  summarise(median_hours = median(resolution_hours), n = n()) |>
  filter(n >= 50)  # only tracts with enough requests to be reliable

# Join to spatial data
tract_map <- nyc_income |>
  left_join(tract_summary, by = "geoid") |>
  st_transform(crs = 4326)

ggplot(tract_map) +
  geom_sf(aes(fill = median_hours), color = NA) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    trans = "log10",
    labels = scales::comma,
    name = "Median Hours\n(log scale)"
  ) +
  labs(
    title = "Median 311 Resolution Time by Census Tract",
    subtitle = "NYC, 2022–2025",
    caption = "Tracts with fewer than 50 requests excluded"
  ) +
  theme_void() +
  theme(legend.position = "right")

# ── 2. Choropleth: median household income by census tract ────────────────────

ggplot(tract_map) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c(
    option = "viridis",
    labels = scales::dollar,
    name = "Median Income"
  ) +
  labs(
    title = "Median Household Income by Census Tract",
    subtitle = "NYC, 2019–2023 ACS",
  ) +
  theme_void() +
  theme(legend.position = "right")

# ── 3. Coefficient plot: bootstrap results ────────────────────────────────────

boot_ci |>
  mutate(
    term = recode(term,
                  "income_quintile2" = "Q2",
                  "income_quintile3" = "Q3",
                  "income_quintile4" = "Q4",
                  "income_quintile5" = "Q5 (Highest)"
    )
  ) |>
  ggplot(aes(x = term, y = mean_estimate, ymin = ci_lower, ymax = ci_upper)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbar(width = 0.2, color = "steelblue") +
  geom_point(size = 3, color = "steelblue") +
  labs(
    title = "Effect of Income Quintile on Log Resolution Time",
    subtitle = "Relative to Q1 (Lowest Income), controlling for complaint type and borough",
    x = "Income Quintile",
    y = "Estimated Effect (log hours)",
    caption = "Bootstrap 95% CI, 200 iterations, n=10,000 sample"
  ) +
  theme_minimal()

# ── 4. Scatter: tract-level median income vs median resolution time ───────────

tract_summary_income <- df |>
  filter(!is.na(geoid), !is.na(median_income)) |>
  group_by(geoid, median_income) |>
  summarise(median_hours = median(resolution_hours),
            n = n(), .groups = "drop") |>
  filter(n >= 50)

ggplot(tract_summary_income,
       aes(x = median_income, y = median_hours)) +
  geom_point(alpha = 0.3, size = 0.8, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Tract-Level Income vs. Median 311 Resolution Time",
    subtitle = "Each point is one census tract (min. 50 requests)",
    x = "Median Household Income",
    y = "Median Resolution Time (hours, log scale)",
    caption = "Loess smoothing line with 95% CI"
  ) +
  theme_minimal()