library(readr)
library(here)
library(DBI)
library(duckdb)

# 1. Remove existing DuckDB file (if it exists)
if (file.exists("fishpond.duckdb")) {
  file.remove("fishpond.duckdb")
}

# 2. Read CSV
data <- read_csv(here("cleaned_data/master_data_pivot.csv"))

# 3. Create a new DuckDB database
con <- dbConnect(duckdb::duckdb(), dbdir = "fishpond.duckdb", read_only = FALSE)

# 4. Write table
dbWriteTable(con, "sensor_data", data)

# 5. Disconnect
dbDisconnect(con, shutdown = TRUE)
