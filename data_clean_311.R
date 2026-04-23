# Step 3 - Clean and filter the raw 311 data, compute resolution time, and 
# prepare for spatial join

library(tidyverse)
library(janitor)

# Load raw 311 data
raw_311 <- readRDS("data/raw_311_2022_2025.rds")

# Compute resolution time in hours
clean_311 <- raw_311 |>
  mutate(
    resolution_hours = as.numeric(
      difftime(closed_date, created_date, units = "hours")
    )
  ) |>
  # Remove invalid records
  filter(
    !is.na(resolution_hours),       # must have both dates
    resolution_hours > 0,           # no negative or zero times
    resolution_hours < 8760,        # cap at 1 year (likely data errors above this)
    borough != "Unspecified"        # must have a known borough
  ) |>
  # Clean up column types
  mutate(
    borough = str_to_title(borough),
    open_data_channel_type = str_to_title(open_data_channel_type)
  )

message(paste("Rows after cleaning:", nrow(clean_311)))
message(paste("Rows removed:", nrow(raw_311) - nrow(clean_311)))

# Quick sanity checks
summary(clean_311$resolution_hours)
clean_311 |> count(borough, sort = TRUE)

# Save
saveRDS(clean_311, "data/clean_311.rds")
message("Saved to data/clean_311.rds")

# Step 4 - Spatial join with census data to attach median income to each 311 request  

library(sf)

# Load census income data (with tract geometries)
nyc_income <- readRDS("data/nyc_income_tracts.rds")

# Convert 311 data to spatial points
# 311 data uses WGS84 (EPSG:4326), must match census tract CRS
clean_311_sf <- clean_311 |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

message("Converted 311 data to spatial points")

# Make sure CRS matches between the two datasets
nyc_income <- st_transform(nyc_income, crs = 4326)

# Spatial join: attach census tract info to each 311 point
# This finds which census tract each 311 request falls inside
message("Running spatial join — this will take a few minutes...")

clean_311_joined <- st_join(clean_311_sf, nyc_income, join = st_within)

message(paste("Joined rows:", nrow(clean_311_joined)))

# Drop rows that didn't match any census tract
clean_311_joined <- clean_311_joined |>
  filter(!is.na(geoid))

message(paste("Rows with tract match:", nrow(clean_311_joined)))

# Drop geometry column — we don't need it anymore after the join
clean_311_final <- clean_311_joined |>
  st_drop_geometry() |>
  rename(median_income = estimate,
         income_moe    = moe)

# Sanity check
glimpse(clean_311_final)
clean_311_final |> count(reliable, sort = TRUE)

# Save final analysis-ready dataset
saveRDS(clean_311_final, "data/clean_311_final.rds")
message("Saved to data/clean_311_final.rds")
