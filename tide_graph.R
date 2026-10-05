library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(readr)
library(here)

# Load the 10-minute tide data
tide_data <- read_csv(
  here("cleaned_data/honolulu_tide_10min.csv"),
  show_col_types = FALSE
) %>%
  mutate(
    date_time_hst = ymd_hms(date_time_hst, tz = "Pacific/Honolulu")
  )

# Simple static ggplot version
ggplot(tide_data, aes(x = date_time_hst, y = water_level_m)) +
  geom_line(color = "#1B4F72", linewidth = 0.4, alpha = 0.85) +
  labs(
    title = "Honolulu Tide (Station 1612340)",
    subtitle = "Water level relative to MLLW – 10-minute intervals",
    x = "Date and Time (HST)",
    y = "Water Level (m)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )