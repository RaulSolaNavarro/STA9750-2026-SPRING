# install.packages(c("tidycensus", "sf")) 

# Pull median household income by census tract for all NYC counties
nyc_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  year = 2023,
  state = "NY",
  county = c("New York",
             "Kings",
             "Queens",
             "Bronx",
             "Richmond"),
  geometry = TRUE
) |>
  clean_names()

message(paste("Pulled", nrow(nyc_income), "census tracts"))

# Sanity checks
glimpse(nyc_income)
summary(nyc_income$estimate)

# How many tracts have missing income estimates?
nyc_income |> filter(is.na(estimate)) |> nrow()

# What share of tracts have missing income?
missing_pct <- 135 / 2327 * 100
message(paste("Missing income tracts:", round(missing_pct, 1), "%"))

# Flag high margin of error tracts (MOE > 30% of estimate = unreliable)
unreliable_tracts <- nyc_income |>
  filter(!is.na(estimate)) |>
  mutate(cv = moe / estimate * 100) |>
  filter(cv > 30) |>
  nrow()

message(paste("Unreliable tracts (CV > 30%):", unreliable_tracts))

# Look at the distribution of CV values
nyc_income |>
  filter(!is.na(estimate)) |>
  mutate(cv = moe / estimate * 100) |>
  summary()

# How many tracts have CV > 50% (very unreliable)?
nyc_income |>
  filter(!is.na(estimate)) |>
  mutate(cv = moe / estimate * 100) |>
  filter(cv > 50) |>
  nrow()

# Flag high CV tracts for transparency but keep them
# Quintile approach in analysis makes this less of a concern
nyc_income <- nyc_income |>
  mutate(cv = ifelse(!is.na(estimate) & !is.na(moe),
                     moe / estimate * 100,
                     NA),
         reliable = case_when(
           is.na(estimate) ~ "missing",
           cv > 50         ~ "unreliable",
           cv > 30         ~ "noisy",
           TRUE            ~ "reliable"
         ))

# Save
saveRDS(nyc_income, "data/nyc_income_tracts.rds")
message("Saved to data/nyc_income_tracts.rds")