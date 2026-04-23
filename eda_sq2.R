library(tidyverse)

# Load final dataset
df <- readRDS("data/clean_311_final.rds")

# ── 1. Distribution of resolution times ──────────────────────────────────────

# Raw distribution (expect heavy right skew)
summary(df$resolution_hours)

# Log scale gives a much cleaner picture
df |>
  ggplot(aes(x = resolution_hours)) +
  geom_histogram(bins = 100, fill = "steelblue", color = NA) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution of 311 Resolution Times",
    x = "Resolution Time (hours, log scale)",
    y = "Count"
  )

# ── 2. Income distribution across tracts ─────────────────────────────────────

df |>
  distinct(geoid, .keep_all = TRUE) |>
  ggplot(aes(x = median_income)) +
  geom_histogram(bins = 50, fill = "steelblue", color = NA) +
  scale_x_continuous(labels = scales::dollar) +
  labs(
    title = "Distribution of Median Household Income by Census Tract",
    x = "Median Household Income",
    y = "Number of Tracts"
  )

# ── 3. Assign income quintiles ────────────────────────────────────────────────

df <- df |>
  mutate(income_quintile = ntile(median_income, 5))

# Check quintile boundaries
df |>
  group_by(income_quintile) |>
  summarise(
    min_income = min(median_income, na.rm = TRUE),
    max_income = max(median_income, na.rm = TRUE),
    n = n()
  )

# ── 4. Median resolution time by income quintile ─────────────────────────────

df |>
  filter(!is.na(income_quintile)) |>
  group_by(income_quintile) |>
  summarise(
    median_hours = median(resolution_hours),
    mean_hours   = mean(resolution_hours),
    n            = n()
  )

# ── 5. Boxplot: resolution time by income quintile ───────────────────────────

df |>
  filter(!is.na(income_quintile)) |>
  mutate(income_quintile = factor(income_quintile,
                                  labels = c("Q1\n(Lowest)", "Q2", "Q3", "Q4", "Q5\n(Highest)"))) |>
  ggplot(aes(x = income_quintile, y = resolution_hours, fill = income_quintile)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(
    title = "311 Resolution Time by Neighborhood Income Quintile",
    x = "Income Quintile",
    y = "Resolution Time (hours, log scale)",
    fill = "Quintile"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save df with quintiles for use in Step 6
saveRDS(df, "data/clean_311_final.rds")
message("Updated dataset saved with income quintiles")

# ── 6. Resolution time by income quintile, broken down by complaint type ──────

# First, find the top 8 complaint types by volume
top_complaints <- df |>
  filter(!is.na(income_quintile)) |>
  count(complaint_type, sort = TRUE) |>
  slice_head(n = 8) |>
  pull(complaint_type)

# Median resolution time by quintile x complaint type
df |>
  filter(!is.na(income_quintile),
         complaint_type %in% top_complaints) |>
  mutate(
    income_quintile = factor(income_quintile,
                             labels = c("Q1\n(Lowest)", "Q2", "Q3", "Q4", "Q5\n(Highest)")),
    complaint_type = str_wrap(complaint_type, width = 20)
  ) |>
  group_by(income_quintile, complaint_type) |>
  summarise(median_hours = median(resolution_hours), .groups = "drop") |>
  ggplot(aes(x = income_quintile, y = median_hours, fill = income_quintile)) +
  geom_col() +
  facet_wrap(~ complaint_type, scales = "free_y", ncol = 4) +
  scale_fill_brewer(palette = "RdYlGn") +
  labs(
    title = "Median Resolution Time by Income Quintile",
    subtitle = "Top 8 complaint types",
    x = "Income Quintile",
    y = "Median Hours",
    fill = "Quintile"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 7))
