

#' Get tibble with dangerous passes
#'
#' @param df Event data set
#' @param shot_window the time window before a shot within which passes are considered
#' as dangerous
#' @param team name of the team for which to compute dangerous passes
#'
#' @return a tibble containing information on location of dangerous passes
#' @export
#'
#' @examples
#' data(eventData)
#' get_danger_passes(df = eventData, shot_window = 20, team = "Borussia Dortmund")
get_danger_passes <- function(df, shot_window = 15, team, pitch_dim = c(120,80)) {

  # declare an empty tibble
  danger_passes <- tibble()

  for (period in 1:2) {
    # keep only accurate passes by England that were not set pieces in this period
    mask_pass <- df  %>%
      filter(type.name == "Pass", team.name == team,
             is.na(pass.outcome.name), period == period)
    # keep only necessary columns
    passes <- mask_pass %>%
      select(location.x, location.y, pass.end_location.x, pass.end_location.y,
             minute, second, player.name, team.name)
    # keep only Shots by England in this period
    mask_shot <- df %>% filter(team.name == team,
                                   type.name == "Shot", period == period)
    # keep only necessary columns
    shots <- mask_shot %>% select(minute, second) %>% unique()
    # convert time to seconds
    shots <- shots %>% mutate(shot_times  = minute*60+ second)

    # find starts of the window
    shots <- shots %>% mutate(shot_start = shot_times - shot_window)
    # #condition to avoid negative shot starts
    # shots <- shots %>% mutate(shot_start = ifelse(shot_start > 0 , shot_start, (period-1)*45))
    # convert to seconds
    passes <- passes %>% mutate(pass_time = minute*60 + second)
    # check if pass is in any of the windows for this half
    pass_to_shot <- purrr::map_lgl(passes$pass_time,
                                   ~ { any(shots$shot_times - shot_window < .x & .x < shots$shot_times)})


    # keep only danger passes
    danger_passes_period <-  passes[pass_to_shot, ]
    # concatenate dataframe with a previous one to keep danger
    # passes from the whole tournament
    danger_passes <- danger_passes %>% bind_rows(danger_passes_period)

  }
  danger_passes$location.x[danger_passes$location.x >= pitch_dim[1]]  <- pitch_dim[1] -0.001
  danger_passes$location.x[danger_passes$location.x <= 0]  <- 0.001
  danger_passes$location.y[danger_passes$location.y >= pitch_dim[2]]  <- pitch_dim[2] - 0.001
  danger_passes$location.y[danger_passes$location.y <= 0]  <- 0.001
  danger_passes
}


#' Heatmap of dangerous passes
#'
#' @param dfDangerPass A data frame containing information on dangerous passes
#' as returned by `get_danger_passes`
#' @param leftToRight Logical; whether you want the team to play from left to right
#' in the plots
#' @param shot_window the time window before a shot within which passes are considered
#' as dangerous
#' @param xBins the number of bins in x direction
#' @param yBins  the number of bins in y direction
#' @param pitch_dim dimension of the pitch
#' @param ... additional arguments passed to `ggplot`, such as `legend.key.width`,
#' `fill_limits` (limit of fill colour scale)
#'
#' @return A list of ggplots, one with pass positions and a heatmap
#' @export
#'
#' @examples
#'
#' data(eventData)
#' df_dp <- get_danger_passes(df = eventData, shot_window = 20, team = "Borussia Dortmund")
#' plot_dangerpass_heatmap(df_dp, shot_window = 20, fill_limits = c(1, 10))
#' df_dp <- get_danger_passes(df = eventData, shot_window = 20, team = "Bayern Munich")
#' plot_dangerpass_heatmap(df_dp, shot_window = 20, fill_limits = c(1, 15), leftToRight = FALSE)
plot_dangerpass_heatmap <- function(dfDangerPass, leftToRight = TRUE, shot_window,
                                    xBins = 6, yBins = 4, pitch_dim = c(120, 80),
                                    ...) {


  add.args <- list(...)

  team <- unique(dfDangerPass$team.name)
  reverse_scale <- ifelse(leftToRight, scale_y_reverse, scale_x_reverse)

  pointplot <- dfDangerPass %>%
    ggplot(aes(x = location.x , y= location.y))+
    draw_pitch(dimensions = pitch_dim)+
    reverse_scale()+
    geom_point(size = 1)+
    labs(title = paste("Passes made", shot_window, "s before a shot"), subtitle = team)+
    theme_bw()+
    theme(axis.text = element_blank(), axis.title = element_blank(), text = element_text(size = 20))

  x.range <- seq(0, pitch_dim[1], length.out = xBins+1)
  y.range <- seq(0, pitch_dim[2], length.out = yBins+1)

  heatmap <- dfDangerPass %>%
    ggplot(aes(x = location.x , y= location.y))+
    geom_bin_2d(binwidth = c(diff(x.range)[1], diff(y.range)[1]))+
    reverse_scale()+
    draw_pitch(dimensions = pitch_dim, fill = NA, palette = "bw")+
    scale_fill_gradient(low = "white", high = "red", limits = add.args$fill_limits)+
    labs(title = paste("Danger Pass Heatmap for", team), fill = "Number of passes:",
         subtitle =  paste("Passes made", shot_window, "s before a shot"))+
    theme_bw()+
    theme(legend.position = "bottom", axis.title = element_blank(), axis.text = element_blank(),
          legend.key.width = add.args$legend.key.width, text = element_text(size = 20))

  list(pointplot = pointplot, heatmap = heatmap)

}

