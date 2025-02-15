# earthquake_R

**earthquake_R** is an R package designed to help you clean, analyze, and visualize earthquake data obtained from NOAA. It provides a suite of functions to transform raw earthquake data into a tidy format and create compelling visualizations including interactive maps and timeline plots.

## Features

- **Data Cleaning**
  - `eq_clean_data`: Cleans raw NOAA earthquake data by converting key columns to numeric, creating a standardized `DATE` column (from `YEAR`, `MONTH`, and `DAY`), and reordering columns.
  - `eq_location_clean`: Processes the `LOCATION_NAME` field by stripping out country prefixes and converting location names to title case.

- **Timeline Plotting**
  - `geom_timeline`: A ggplot2 geom for plotting earthquake timelines.
  - `geom_timeline_label`: Adds annotations to highlight the largest earthquake events on a timeline.
  - `theme_timeline`: A custom ggplot2 theme to enhance the appearance of timeline plots.

- **Interactive Mapping**
  - `eq_map`: Creates an interactive Leaflet map showing earthquake epicenters with circle markers sized by magnitude.
  - `eq_map_add_popup`: Adds a column with HTML-formatted popup text for enhanced map annotations.
  - `eq_create_label`: Generates custom HTML popup labels by combining details like location, magnitude, and total deaths.

## Installation

You can install **earthquake_R** directly from GitHub using the `devtools` package:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install earthquake_R from GitHub
devtools::install_github("your_username/earthquake_R")


install.packages(c("dplyr", "lubridate", "ggplot2", "leaflet", "readr", "rlang", "grid"))


library(earthquake_R)
library(readr)

# Load raw NOAA earthquake data
raw_data <- readr::read_delim("path/to/earthquakes.tsv.gz", delim = "\t")

# Clean the raw data
clean_data <- eq_clean_data(raw_data)
head(clean_data)

library(ggplot2)
library(dplyr)
library(stringr)

# Generate a timeline plot
clean_data %>%
  mutate_at(vars(DEATHS, EQ_PRIMARY), as.numeric) %>%
  filter(str_detect(tolower(COUNTRY), "china|usa$|pakistan")) %>%
  filter(DATE >= "2000-01-01") %>%
  ggplot(aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, fill = DEATHS)) +
  geom_timeline() +
  geom_timeline_label(aes(label = LOCATION_NAME, n_max_var = DEATHS), n_max = 5) +
  theme_timeline()

# Generate HTML popup labels
map_data <- map_data %>%
  dplyr::mutate(popup_text = eq_create_label(.))

# Create the map with custom labels
eq_map(data = map_data, annot_col = "popup_text")

browseVignettes("earthquake_R")
