# Earthquake Visualization Tools

This repository contains a collection of R scripts for cleaning, analyzing, and visualizing earthquake data from NOAA. It provides a set of functions for:

- **Data Cleaning**: Convert and tidy NOAA earthquake datasets.
  - `eq_clean_data`: Cleans the raw NOAA dataset (e.g., converting columns to numeric, creating a Date column).
  - `eq_location_clean`: Cleans the `LOCATION_NAME` column by removing extraneous text and formatting the location names.
- **Timeline Plotting**: Create timeline plots of earthquake events.
  - `geom_timeline`: A ggplot2 geom for plotting earthquake timelines.
  - `geom_timeline_label`: Adds labels to the largest earthquakes on the timeline.
  - `theme_timeline`: A custom ggplot2 theme for timeline plots.
- **Interactive Mapping**: Map earthquake epicenters on an interactive leaflet map.
  - `eq_map`: Plots earthquakes on an interactive map with circle markers sized by magnitude.
  - `eq_map_add_popup`: Adds a popup text column for annotations on the map.

## Installation

Clone or download this repository:

```bash
git clone https://github.com/your_username/your_repo_name.git
