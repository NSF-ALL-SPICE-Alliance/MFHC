
library(readr)
library(dplyr)
library(here)
library(DBI)
library(duckdb)

# Load the shared timestamp helper
source(here("R", "time_helpers.R"))

# Read the master CSV timestamps as text first
data <- read_csv(
  here("cleaned_data", "master_data_pivot.csv"),
  col_types = cols(
    date_time_hst = col_character()
  )
)

# Convert UTC CSV timestamps to Hawaii wall-clock time
data <- data %>%
  mutate(
    date_time_hst = utc_to_hst_wall_time(date_time_hst)
  )

# IMPORTANT:
# Stop the Shiny app and close any existing database connections
# before replacing the database file.

# Remove the existing database
db_path <- here("fishpond.duckdb")

if (file.exists(db_path)) {
  file.remove(db_path)
}

# Create the database
con <- dbConnect(
  duckdb::duckdb(),
  dbdir = db_path,
  read_only = FALSE
)

# Write corrected sensor data
dbWriteTable(
  con,
  "sensor_data",
  data
)

# Verify the stored timestamps
check_times <- dbGetQuery(con, "
  SELECT
    CAST(date_time_hst AS VARCHAR) AS timestamp_stored,
    site,
    variable,
    value
  FROM sensor_data
  ORDER BY date_time_hst
  LIMIT 10
")

print(check_times)

# Close connection
dbDisconnect(con, shutdown = TRUE)

