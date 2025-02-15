% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_timeline.R
\docType{package}
\name{geom_timeline}
\alias{geom_timeline}
\title{Timeline geom for plotting earthquake events in earthquake_R}
\usage{
geom_timeline(mapping = NULL, data = NULL, stat = "identity",
              position = "identity", na.rm = TRUE,
              show.legend = NA, inherit.aes = TRUE, ...)
}
\description{
\code{geom_timeline()} is a ggplot2 geom provided by the \strong{earthquake_R} package.
It plots a timeline of earthquake events where the x-axis represents dates and the y-axis 
can be used for stratification (e.g., by country). Each earthquake is represented by a point 
on the timeline, with customizable aesthetics such as color, size, and transparency.
}
\details{
This geom utilizes the \code{GeomTimeline} ggproto object to generate the timeline line and 
the earthquake markers. The timeline is rendered as a horizontal line spanning the range of 
the provided dates, and each earthquake event is plotted as a point along this line. This function 
can be combined with other plotting functions within the \strong{earthquake_R} package for enhanced 
visualizations such as annotated timelines and thematic plots.
}
\seealso{
\code{\link{geom_timeline_label}}, \code{\link{theme_timeline}}
}
\examples{
library(ggplot2)
library(dplyr)
library(stringr)

# Load and clean example earthquake data
raw_data <- readr::read_delim(
  system.file("extdata/earthquakes.tsv.gz", package = "earthquake_R"), 
  delim = "\t"
)
clean_data <- eq_clean_data(raw_data)

# Create a timeline plot
g <- clean_data %>%
  mutate_at(vars(DEATHS, EQ_PRIMARY), as.numeric) %>%
  filter(str_detect(tolower(COUNTRY), "china|usa$|pakistan")) %>%
  filter(DATE >= "2000-01-01") %>%
  ggplot(aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, fill = DEATHS))
g + geom_timeline()
}
\keyword{plot}
