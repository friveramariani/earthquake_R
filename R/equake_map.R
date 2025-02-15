#' Plot earthquakes on an interactive map
#'
#' @description The function maps the epicenters (LATITUDE/LONGITUDE) and
#'   annotates each point with a pop up window containing annotation data stored
#'   in a column of the data frame. Each earthquake is shown with a circle, and
#'   the radius of the circle is proportional to the earthquake’s magnitude
#'   (EQ_PRIMARY).
#'
#' @param df a data frame containing the filtered data with earthquakes to
#'   visualize.
#' @param annot_col the name of the column used for the annotation in
#'   the pop-up. Defaults to "DATE".
#' @param provider a character string specifying the tile provider.
#'   Defaults to "Esri.NatGeoWorldMap".
#'
#' @return a \code{leaflet} type HTML widget.
#' @export
#' @importFrom dplyr mutate
#' @importFrom leaflet leaflet addProviderTiles addCircleMarkers
#'
#' @examples
#' library(dplyr)
#'
#' # load data
#' filename <- system.file("extdata/earthquakes.tsv.gz", package = "quakeR")
#' raw_data <- readr::read_delim(filename, delim = "\t")
#'
#' map_data <-
#'   raw_data %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO",
#'                 lubridate::year(DATE) >= 2000)
#'
#' eq_map(map_data)
#' eq_map(map_data, annot_col = "EQ_PRIMARY")
#' eq_map(map_data, annot_col = "LOCATION_NAME")
#'
eq_map <-
  function(df, annot_col = "DATE", provider = "Esri.NatGeoWorldMap"){

    if(!annot_col %in% names(df)){
      stop(paste0("Annotation column '", annot_col, "' not in the data."))
    }

    df %>%
      dplyr::mutate(popup = as.character(.[[annot_col]])) %>%
      leaflet::leaflet(width = "100%") %>%
      leaflet::addProviderTiles(provider = provider) %>%
      leaflet::addCircleMarkers(
        lng = ~LONGITUDE, lat = ~LATITUDE,
        radius = ~ sqrt(EQ_PRIMARY) * 4,
        popup = ~ popup,
        weight = 1,
        fillColor = "#FF6347", fillOpacity = 0.60)
  }


#' Add a column with popup text to be displayed on the map
#'
#' @description This function takes the dataset as an argument and creates an
#'   HTML label that can be used as the annotation text in the leaflet map. This
#'   function puts together a character string for each earthquake that shows
#'   the cleaned location (as cleaned by the \code{eq_location_clean()}
#'   function), the magnitude (\code{EQ_PRIMARY}), and the total number of deaths
#'   (\code{TOTAL_DEATHS}). If an earthquake is missing values for any of these,
#'   both the label and the value are skipped for that element of the tag.
#'
#' @param eq_data a dataframe with NOAA's earthquake data.
#'
#' @return The original dataframe with a new column called \code{popup_text}.
#' @export
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' # load data
#' filename <- system.file("extdata/earthquakes.tsv.gz", package = "quakeR")
#' raw_data <- readr::read_delim(filename, delim = "\t")
#'
#' map_data <-
#'   raw_data %>%
#'
