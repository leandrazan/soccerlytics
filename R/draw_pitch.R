## copied from scoutr
#' annotate a pitch to a ggplot object
#'
#' copied from package scoutr
#'
#' @param dimensions pitch dimensions
#' @param palette colour palette, one of "gw", "bw", "classic", "smurf", "dark",
#' "wc"
#' @param color line colour
#' @param fill fill colour
#' @param coord_flip whether to flip coordinates
#' @param reverse_scale whether in the ggplot object to which the pitch is annotated
#' any scale was reversed. NULL if this is not the case, anything else if it is the case
#'
#' @return a list with ggplot commands
#' @export
#'
#' @examples
#' ggplot() + draw_pitch()
draw_pitch <- function (dimensions = c(120, 80), palette = "gw", color = NULL,
          fill = NULL, coord_flip = FALSE, reverse_scale = NULL) {

  if(!is.null(reverse_scale)) {
    coord_flip_box_arc <- TRUE
  } else {coord_flip_box_arc <- FALSE}
  palette_color <- switch(palette, gw = "grey60", classic = "white",
                          smurf = "white", dark = "white", wc = "#ffffff",
                          bw = "#130a06")
  palette_fill <- switch(palette, gw = "white", classic = "#196f0c",
                         smurf = "#0033A0", dark = "#130a06", wc = "#8d1b3d",
                         bw = "white")
  if (!is.null(color)) {
    palette_color <- color
  }
  if (!is.null(fill)) {
    palette_fill <- fill
  }
  x_min <- 0
  y_min <- 0
  x_max <- dimensions[1]
  y_max <- dimensions[2]
  x_mid <- (x_max - x_min)/2
  y_mid <- (y_max - y_min)/2
  x_adj <- x_max/105
  y_adj <- y_max/70
  fcn_list <- list(boundary = "annotate", center_circle = "annotate",
                   center_point = "annotate", center_line = "annotate",
                   left_penalty_box = "annotate", right_penalty_box = "annotate",
                   left_goal_box = "annotate", right_goal_box = "annotate",
                   left_penalty_dot = "annotate", right_penalty_dot = "annotate",
                   left_goal = "annotate", right_goal = "annotate",
                   left_box_arc = "annotate", right_box_arc = "annotate",
                   lower_left_arc = "annotate", upper_left_arc = "annotate",
                   lower_right_arc = "annotate", upper_right_arc = "annotate")
  arg_list <- list(boundary = list(geom = "rect", xmin = x_min,
                                   xmax = x_max, ymin = y_min, ymax = y_max, color = palette_color,
                                   fill = palette_fill, size = 1.5),
                   center_circle = list(geom = "path",  x = x_mid + (9.15 * x_adj) * cos(seq(0, 2 * pi, length.out = 100)),
                                        y = y_mid + (9.15 * y_adj) * sin(seq(0, 2 * pi, length.out = 100)),
                                        color = palette_color), center_point = list(geom = "point",  x = x_mid, y = y_mid, size = 2, color = palette_color),
                   center_line = list(geom = "segment", x = x_mid,
                                      y = y_min, xend = x_mid, yend = y_max, color = palette_color),
                   left_penalty_box = list(geom = "rect", xmin = x_min,
                                           xmax = 16.5 * x_adj, ymin = (y_max - (40.3 * y_adj))/2,
                                           ymax = (y_max - (40.3 * y_adj))/2 + (40.3 * y_adj),
                                           color = palette_color, fill = palette_fill, alpha = 0),
                   right_penalty_box = list(geom = "rect", xmin = x_max -
                                              (16.5 * x_adj), xmax = x_max,
                                            ymin = (y_max - (40.3 * y_adj))/2, ymax = (y_max - (40.3 * y_adj))/2 + (40.3 *  y_adj),
                                            color = palette_color, fill = palette_fill,
                                            alpha = 0),
                   left_goal_box = list(geom = "rect", xmin = 0, xmax = 5.5 * x_adj, ymin = (y_max - (40.3 * y_adj))/2 + (11 * y_adj),
                                        ymax = (y_max - (40.3 *  y_adj))/2 + (29.3 * y_adj), color = palette_color,
                                        fill = palette_fill, alpha = 0),
                   right_goal_box = list(geom = "rect", xmin = x_max - (5.5 * x_adj), xmax = x_max, ymin = (y_max - (40.3 * y_adj))/2 + (11 * y_adj),
                                         ymax = (y_max - (40.3 * y_adj))/2 + (29.3 * y_adj), color = palette_color,
                                         fill = palette_fill, alpha = 0),
                   left_penalty_dot = list(geom = "point",  x = 11 * x_adj, y = y_mid, size = 2, color = palette_color),
                   right_penalty_dot = list(geom = "point", x = x_max -
                                              (11 * x_adj), y = y_mid, size = 2, color = palette_color),
                   left_goal = list(geom = "rect", xmin = x_min -
                                      (2.4 * x_adj), xmax = x_min, ymin = (y_max - (40.3 * y_adj))/2 + (16.5 * y_adj),
                                    ymax = (y_max - (40.3 *  y_adj))/2 + (23.3 * y_adj),
                                    color = palette_color,
                                    fill = palette_color),
                   right_goal = list(geom = "rect", xmin = x_max, xmax = x_max + (2.4 * x_adj),
                                     ymin = (y_max - (40.3 * y_adj))/2 + (16.5 * y_adj),
                                     ymax = (y_max -  (40.3 * y_adj))/2 + (23.3 * y_adj), color = palette_color,
                                     fill = palette_color),
                   left_box_arc = list(geom = "curve",  x = 16.5 * x_adj, xend = 16.5 * x_adj,
                                       y = y_mid - (ifelse(coord_flip_box_arc, -1, 1) * sqrt((y_adj * 5.5)^2 + (y_adj * 9.15)^2)),
                                       yend = y_mid + (ifelse(coord_flip_box_arc,-1, 1) * sqrt((y_adj * 5.5)^2 + (y_adj * 9.15)^2)),
                                       color = palette_color),
                   right_box_arc = list(geom = "curve",  x = x_max - (16.5 * x_adj), xend = x_max - (16.5 * x_adj),
                                        y = y_mid + (ifelse(coord_flip_box_arc, -1, 1) * sqrt((y_adj * 5.5)^2 + (y_adj * 9.15)^2)),
                                        yend = y_mid - (ifelse(coord_flip_box_arc, -1, 1) * sqrt((y_adj * 5.5)^2 +  (y_adj * 9.15)^2)),
                                        color = palette_color),
                   lower_left_arc = list(geom = "curve", x = ifelse(coord_flip,
                                                                    x_min, x_min + 2 * x_adj),
                                         xend = ifelse(coord_flip,  x_min + 2 * x_adj, x_min),
                                         y = ifelse(coord_flip,  y_min + 2 * y_adj, y_min),
                                         yend = ifelse(coord_flip, y_min, y_min + 2 * y_adj), color = palette_color),
                   upper_left_arc = list(geom = "curve", x = ifelse(coord_flip,
                                                                    x_min + 2 * x_adj, x_min),
                                         xend = ifelse(coord_flip,  x_min, x_min + 2 * x_adj),
                                         y = ifelse(coord_flip, y_max, y_max - 2 * y_adj),
                                         yend = ifelse(coord_flip, y_max - 2 * y_adj, y_max), color = palette_color),
                   lower_right_arc = list(geom = "curve", x = ifelse(coord_flip,
                                                                     x_max - 2 * x_adj, x_max),
                                          xend = ifelse(coord_flip, x_max, x_max - 2 * x_adj),
                                          y = ifelse(coord_flip, y_min, y_min + 2 * y_adj),
                                          yend = ifelse(coord_flip, y_min + 2 * y_adj, y_min), color = palette_color),
                   upper_right_arc = list(geom = "curve",
                                          x = ifelse(coord_flip, x_max, x_max - 2 * x_adj),
                                          xend = ifelse(coord_flip,  x_max - 2 * x_adj, x_max),
                                          y = ifelse(coord_flip, y_max - 2 * y_adj, y_max),
                                          yend = ifelse(coord_flip,  y_max, y_max - 2 * y_adj), color = palette_color))
  purrr::invoke_map(fcn_list, arg_list)
}
