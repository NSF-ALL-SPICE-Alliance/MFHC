library(httr)
library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)
library(tibble)
library(here)
library(readr)

get_noaa_water_level <- function(begin_date, end_date,
                                 station = "1612340",
                                 datum = "MLLW",
                                 units = "metric",
                                 time_zone = "lst_ldt") {
  
  url <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
  query <- list(
    begin_date  = begin_date,
    end_date    = end_date,
    station     = station,
    product     = "water_level",
    datum       = datum,
    units       = units,
    time_zone   = time_zone,
    format      = "json",
    application = "MaunaluaFishpond"
  )
  
  resp <- GET(url, query = query)
  stop_for_status(resp)
  
  parsed <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  
  if (is.null(parsed$data) || length(parsed$data) == 0) {
    warning("No data for ", begin_date, " – ", end_date)
    return(tibble())
  }
  
  tibble(
    date_time_hst = ymd_hm(parsed$data$t, tz = "Pacific/Honolulu"),  # ← key fix
    water_level_m = as.numeric(parsed$data$v)
  )
}

# Adjust the start date to match the earliest sensor data you care about
starts <- seq(as.Date("2023-02-09"), as.Date("2025-10-05"), by = "month")
ends   <- pmin(starts + months(1) - days(1), as.Date("2025-10-24"))

tide_raw <- map2_dfr(starts, ends, ~ {
  message("Fetching ", .x, " → ", .y)
  get_noaa_water_level(format(.x, "%Y%m%d"), format(.y, "%Y%m%d"))
})

# Aggregate to exact 10-minute grid
tide_10min <- tide_raw %>%
  mutate(date_time_hst = floor_date(date_time_hst, "10 minutes")) %>%
  group_by(date_time_hst) %>%
  summarise(water_level_m = mean(water_level_m, na.rm = TRUE), .groups = "drop")

# Quick check
glimpse(tide_10min)
summary(tide_10min$water_level_m)

# Save
write_csv(tide_10min, here("cleaned_data/honolulu_tide_10min.csv"))
