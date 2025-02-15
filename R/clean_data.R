#' Clean NOAA dataset
#'
#' Takes raw NOAA data frame and returns a clean data frame. The clean data
#' frame has the following: A date column created by uniting the year,
#' month, day and converting it to the Date class; LATITUDE, LONGITUDE,
#' EQ_PRIMARY, TOTAL_DEATHS, and FOCAL_DEPTH columns converted to numeric class.
#'
#' @param raw raw NOAA data frame
#'
#' @return a clean data frame
#'
#' @importFrom dplyr %>% select mutate mutate_at if_else everything vars funs
#' @importFrom lubridate make_date
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' clean_data <- eq_clean_data(data)
#' }
#'
eq_clean_data <-
    function(raw){
        df <-
            raw %>%
            dplyr::mutate_at(
                dplyr::vars("LATITUDE", "LONGITUDE",
                            "EQ_PRIMARY", "TOTAL_DEATHS", "FOCAL_DEPTH"),
                as.numeric
            ) %>%
            dplyr::mutate_at(
                dplyr::vars("MONTH", "DAY"),
                dplyr::funs(dplyr::if_else(is.na(.), 1L, .))
            ) %>%
            dplyr::mutate(
                DATE = lubridate::make_date(
                    year = .data$YEAR,
                    month = .data$MONTH,
                    day = .data$DAY
                ),
                LOCATION_NAME = eq_location_clean(.data$LOCATION_NAME)
            ) %>%
            dplyr::select(
                .data$DATE, .data$LATITUDE, .data$LONGITUDE,
                .data$LOCATION_NAME, .data$COUNTRY,
                .data$EQ_PRIMARY, .data$FOCAL_DEPTH, .data$DEATHS,
                dplyr::everything()
            )
        
        return(df)
    }


#' Cleans the LOCATION_NAME column of NOAA dataset
#'
#' This function cleans the LOCATION_NAME column by stripping out the country
#' name (including the colon) and converts names to title case (as opposed to
#' all caps).
#'
#' @param x character vector from LOCATION_NAME column of NOAA dataset
#'
#' @return character vector of clean location names
#' @importFrom dplyr %>%
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' clean_data <- eq_location_clean(data)
#' }
#'
eq_location_clean <-
    function(x){
        gsub("^.+\\: ", "", x) %>%
            tolower() %>%
            gsub("(^|[[:space:]-\\(,])([[:alpha:]])", "\\1\\U\\2", ., perl = TRUE) %>%
            trimws()
    }


## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
