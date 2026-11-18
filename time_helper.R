
library(lubridate)

# Convert UTC timestamps into Hawaii local clock time.
# Store the result as a timezone-free-style timestamp
# so DuckDB and the Shiny app can use the same convention.

utc_to_hst_wall_time <- function(x) {
  
  # If timestamps came from a CSV as text, parse them as UTC
  if (is.character(x)) {
    x <- ymd_hms(x, tz = "UTC", quiet = TRUE)
  }
  
  # Convert the actual instant to Hawaii local time
  hst_time <- with_tz(x, tzone = "Pacific/Honolulu")
  
  # Preserve the Hawaii clock reading while removing
  # timezone-dependent display behavior.
  as.POSIXct(
    format(
      hst_time,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Pacific/Honolulu"
    ),
    tz = "UTC"
  )
}

