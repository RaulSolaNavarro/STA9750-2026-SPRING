library(tidyverse)
library(janitor)

pull_311_year <- function(year) {
  message(paste("Pulling", year, "..."))
  
  base_url <- "https://data.cityofnewyork.us/resource/erm2-nwe9.csv"
  
  select_clause <- "$select=unique_key,created_date,closed_date,complaint_type,descriptor,agency,status,incident_zip,borough,latitude,longitude,open_data_channel_type"
  
  where_clause <- paste0(
    "$where=status='Closed'",
    " AND created_date >= '", year, "-01-01T00:00:00'",
    " AND created_date <= '", year, "-12-31T23:59:59'",
    " AND latitude IS NOT NULL",
    " AND longitude IS NOT NULL"
  )
  
  url <- paste0(
    base_url, "?",
    URLencode(select_clause, repeated = TRUE), "&",
    URLencode(where_clause, repeated = TRUE), "&",
    "$limit=5000000"
  )
  
  df <- read_csv(url, show_col_types = FALSE)
  message(paste("  -->", nrow(df), "rows for", year))
  return(df)
}

# Pull each year separately
raw_2022 <- pull_311_year(2022)
raw_2023 <- pull_311_year(2023)
raw_2024 <- pull_311_year(2024)
raw_2025 <- pull_311_year(2025)

# Combine into one dataframe
raw_311 <- bind_rows(raw_2022, raw_2023, raw_2024, raw_2025)

message(paste("Total rows:", nrow(raw_311)))

# Save
saveRDS(raw_311, "data/raw_311_2022_2025.rds")
message("Saved to data/raw_311_2022_2025.rds")

# Sanity checks
range(raw_311$created_date)
raw_311 |> count(borough, sort = TRUE)
raw_311 |> count(complaint_type, sort = TRUE) |> head(20)