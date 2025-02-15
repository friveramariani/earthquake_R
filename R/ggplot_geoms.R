#' Plot earthquake timelines
#'
#' @description \code{geom_timeline()} adds the timeline geom to a ggplot2 plot,
#'   \code{geom_timeline_label()} adds labels to the largest earthquakes, and
#'   \code{theme_timeline()} applies formatting to timeline plots. See
#'   "Details".
#'
#' @details \code{geom_timeline()} is a ggplot2 geom for plotting a time line of
#'   earthquakes ranging from xmin to xmax dates with a point for each
#'   earthquake. Optional aesthetics include color, size, and alpha (for
#'   transparency). The x aesthetic is a date and an optional y aesthetic is a
#'   factor indicating some stratification in which case multiple time lines
#'   will be plotted for each level of the factor (e.g. country).
#'
#' @inheritParams ggplot2::geom_point
#'
#' @importFrom ggplot2 layer
#' @export
geom_timeline <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "identity", na.rm = TRUE,
           show.legend = NA, inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = GeomTimeline, mapping = mapping,
      data = data, stat = stat, position = position,
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }


#' @rdname geom_timeline
#'
#' @details \code{geom_timeline_label()} is used for adding annotations to the
#'   earthquake data. This geom adds a vertical line to each data point with a
#'   text annotation (e.g. the location of the earthquake) attached to each
#'   line. Aesthetics are x, which is the date of the earthquake and label which
#'   takes the column name from which annotations will be obtained.
#'
#' @param n_max an optional integer to label n_max number of largest (by
#'   magnitude) earthquakes.
#' @inheritParams ggplot2::geom_text
#'
#' @importFrom ggplot2 layer
#' @export
geom_timeline_label <-
  function(mapping = NULL, data = NULL,
           stat = "identity", position = "identity", ...,
           na.rm = TRUE, n_max = NULL, show.legend = NA,
           inherit.aes = TRUE) {

    ggplot2::layer(
      geom = GeomTimelineLabel, mapping = mapping,
      data = data, stat = stat, position = position,
      show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, n_max = n_max, ...)
    )
  }


#' @rdname geom_timeline
#'
#' @inheritParams ggplot2::theme_classic
#'
#' @importFrom ggplot2 theme_classic theme element_blank
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(stringr)
#'
#' # load data
#' filename <- system.file("extdata/earthquakes.tsv.gz", package = "quakeR")
#' raw_data <- readr::read_delim(filename, delim = "\t")
#' clean_data <- eq_clean_data(raw_data)
#'
#' # plot timeline
#' clean_data %>%
#'   mutate_at(vars(DEATHS, EQ_PRIMARY), as.numeric) %>%
#'   filter(str_detect(str_to_lower(COUNTRY), "china|usa$|pakistan")) %>%
#'   filter(DATE >= "2000-01-01") %>%
#'   ggplot(aes(x = DATE,
#'              y = COUNTRY,
#'              size = EQ_PRIMARY,
#'              fill = DEATHS))+
#'   geom_timeline()+
#'   geom_timeline_label(aes(label = LOCATION_NAME,
#'                           n_max_var = DEATHS),
#'                       n_max = 5)+
#'   theme_timeline()
#'
theme_timeline <-
  function (base_size = 12, base_family = "") {
    ggplot2::theme_classic(base_size = base_size, base_family = base_family)  +
      ggplot2::theme(legend.position = "top",  # changed from "bottom" to "top"
                     axis.line.y = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank())
  }



#' @keywords internal
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob gList gpar
GeomTimeline <-
  ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                   required_aes = c("x"),

                   default_aes =
                     ggplot2::aes(y = 0.33,
                                  color = "grey",
                                  shape = 21, size = 2, alpha = 0.5,
                                  stroke = 0.25,
                                  fill = "lightblue"),  # changed from "white" to "lightblue"

                   draw_key = ggplot2::draw_key_point,

                   draw_panel = function(data, panel_scales, coord) {
                     ## Transform the data first
                     coords <- coord$transform(data, panel_scales)
                     ## Construct a grid grob for points
                     pts <-
                       grid::pointsGrob(
                         x = coords$x,
                         y = coords$y,
                         pch = coords$shape,
                         size = unit(coords$size/4, units = "char"),
                         gp = grid::gpar(
                           col = coords$color,
                           fill = coords$fill
                         )
                       )

                     lns_y <- unique(coords$y)
                     lns_x <- rep(c(0, 1), each = length(lns_y))

                     lns <-
                       grid::polylineGrob(
                         x =
                           unit(rep(c(0, 1), each = length(lns_y)),
                                units = "npc"),
                         y =
                           unit(c(lns_y, lns_y),
                                units = "npc"),
                         id =
                           rep(seq_along(lns_y), 2),
                         gp =
                           grid::gpar(col = "grey", lwd = .pt))

                     return(grid::gList(lns, pts))
                   })






#' @keywords internal
#' @importFrom ggplot2 ggproto Geom aes draw_key_point
#' @importFrom grid pointsGrob gList gpar
GeomTimelineLabel <-
  ggplot2::ggproto(
    "GeomTimelineLabel", ggplot2::Geom,

    required_aes = c("x", "label"),

    default_aes = aes(n_max_var = NULL),

    draw_key = ggplot2::draw_key_blank,

    setup_data = function(data, params) {
      if (!is.null(params$n_max)) {
        if (!("n_max_var" %in% colnames(data))) {
          stop("The 'n_max_var' aesthetics is required.")
        }
        data <- data %>%
          dplyr::arrange(-n_max_var) %>%
          dplyr::group_by(group) %>%
          dplyr::slice(1:params$n_max) %>%
          dplyr::ungroup()
      }
      data
    },

    draw_panel = function(data, panel_scales, coord, n_max) {

      if (!("y" %in% colnames(data))) {
        data$y <- 0.33
      }

      coords <- coord$transform(data, panel_scales)
      y_nudge <- 0.2 / length(unique(data$group))

      lab <-
        grid::textGrob(
          label = coords$label,
          x = unit(coords$x, "npc"),
          y = unit(coords$y + y_nudge, "npc"),
          just = c("left", "bottom"),
          rot = 30  # changed from 45 to 30 degrees
        )

      ln <-
        grid::polylineGrob(
          x = unit(c(coords$x, coords$x), "npc"),
          y = unit(c(coords$y, coords$y + y_nudge), "npc"),
          id = rep(1:dim(coords)[1], 2),
          gp = grid::gpar(
            col = "grey"
          )
        )

      return(grid::gList(ln, lab))
    }
  )
