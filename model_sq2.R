library(tidyverse)
library(broom)

# Load data
df <- readRDS("data/clean_311_final.rds")

# ── 1. Prepare modeling dataset ───────────────────────────────────────────────

model_df <- df |>
  filter(!is.na(income_quintile),
         !is.na(resolution_hours)) |>
  mutate(
    log_resolution = log(resolution_hours),
    income_quintile = factor(income_quintile),
    complaint_type  = factor(complaint_type),
    borough         = factor(borough)
  )

message(paste("Modeling dataset rows:", nrow(model_df)))

# ── 2. Base model: income quintile only ──────────────────────────────────────

model_base <- lm(log_resolution ~ income_quintile, data = model_df)
summary(model_base)

# ── 3. Full model: controlling for complaint type and borough ─────────────────

model_full <- lm(log_resolution ~ income_quintile + complaint_type + borough,
                 data = model_df)

# Just show the income quintile coefficients — complaint type has 200+ levels
tidy(model_full) |>
  filter(str_detect(term, "income_quintile")) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

# ── 4. Interpret the coefficients ────────────────────────────────────────────

# Convert log coefficients to % difference relative to Q1
tidy(model_full) |>
  filter(str_detect(term, "income_quintile")) |>
  mutate(
    pct_difference = round((exp(estimate) - 1) * 100, 1)
  ) |>
  select(term, estimate, pct_difference, p.value)

# ── 5. Bootstrap confidence intervals (faster version) ───────────────────────

set.seed(42)
boot_df <- model_df |> slice_sample(n = 10000)  # smaller sample

boot_results <- map_dfr(1:200, function(i) {     # fewer iterations
  sample_data <- boot_df |> slice_sample(prop = 1, replace = TRUE)
  fit <- lm(log_resolution ~ income_quintile + complaint_type + borough,
            data = sample_data)
  tidy(fit) |>
    filter(str_detect(term, "income_quintile")) |>
    mutate(iteration = i)
})

message("Bootstrap complete")

boot_ci <- boot_results |>
  group_by(term) |>
  summarise(
    mean_estimate = mean(estimate),
    ci_lower = quantile(estimate, 0.025),
    ci_upper = quantile(estimate, 0.975)
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 4)))

print(boot_ci)

saveRDS(boot_ci, "data/boot_ci.rds")
message("Saved bootstrap results")

# Save pre-computed results for use in Quarto report
saveRDS(tidy(model_full) |>
          filter(str_detect(term, "income_quintile")) |>
          mutate(pct_difference = round((exp(estimate) - 1) * 100, 1)),
        "data/model_results.rds")

# Pre-compute tract summary for maps
tract_summary <- df |>
  filter(!is.na(geoid)) |>
  group_by(geoid) |>
  summarise(median_hours = median(resolution_hours), n = n()) |>
  filter(n >= 50)
saveRDS(tract_summary, "data/tract_summary.rds")

# Pre-compute quintile summary
quintile_summary <- df |>
  filter(!is.na(income_quintile)) |>
  group_by(income_quintile) |>
  summarise(median_hours = median(resolution_hours),
            mean_hours = mean(resolution_hours),
            n = n())
saveRDS(quintile_summary, "data/quintile_summary.rds")