## functions for plotting attacking zones

# e.g. for pitch dimension 120 x 80:
# final third: x >= 80
# zone 1: y \in [0, 20]
# zone 2: y \in (20, 40]
# zone 3: y \in (40, 60]
# zone 2: y \in (60, 80]



get_zone_final_third <- function(x, y, pitchLength = 120, pitchWidth = 80) {

  final_third_start <- pitchLength*2/3

  zone_borders <- seq(0, pitchWidth, pitchWidth/4)


  if(x < final_third_start) {
    return(NA)
  } else {
    if (y >= zone_borders[1] & y <= zone_borders[2]) {
      return(1)
    } else if (y > zone_borders[2] & y <= zone_borders[3]) {
      return(2)
    } else if (y > zone_borders[3] & y <= zone_borders[4]) {
      return(3)
    } else if (y > zone_borders[4] & y <= zone_borders[5]) {
      return(4)
    } else {
      return(NA)
    }
  }
  zone
}


#' Compute distribution of attacks on zones
#'
#' @param df event data set
#' @param n_zones Number of partitions of final third
#' @param pitchLength Length of pitch in data set
#' @param pitchWidth Width of pitch in data set
#'
#' @return A dataframe containing zone borders and attacks per zone for both teams
#' @export
#'
#' @details An attack is every passing or carrying event that starts in the first
#' two thirds and ends up in any zone of the final third. Here, the end location
#' of the event determines to which zone the attack is assigned.
#' @examples
#' data(eventData)
#' compute_attacking_zones(df = eventData)
compute_attacking_zones <- function(df, n_zones = 4, pitchLength = 120, pitchWidth = 80) {


  df <- df %>% dplyr::select(id, team.name, player.name, type.name,
                             contains("outcome.name"), possession_team.name, minute, second,
                             location.x, location.y, contains("end_location"),
                             starts_with("obv")) %>%
    unique()

  ## only end locations are relevant, so filter events with start locations not in
  ## final third and rename end locations
  df_carries <- df %>% dplyr::filter(type.name == "Carry") %>%
    dplyr::filter(location.x <= pitchLength*2/3) %>%
    dplyr::rename(end.location.x = carry.end_location.x, end.location.y = carry.end_location.y)

  df_passes <- df %>% dplyr::filter(type.name == "Pass", is.na(pass.outcome.name)) %>%
    dplyr::filter(location.x <= pitchLength*2/3) %>%
    dplyr::rename(end.location.x = pass.end_location.x, end.location.y = pass.end_location.y)

  df <- dplyr::bind_rows(df_passes, df_carries) %>%
    dplyr::mutate(zone = purrr::map2_dbl(end.location.x, end.location.y,
                                         ~ get_zone_final_third(x = .x, y = .y,
                                                                pitchLength = pitchLength, pitchWidth = pitchWidth)))

  df_counts <- df %>% filter(!is.na(zone)) %>% dplyr::group_by(team.name, zone) %>%
    summarise(n = n(), .groups = "drop")

  df_counts <- df_counts %>%
    dplyr::left_join(df_counts %>% dplyr::group_by(team.name) %>%
                       dplyr::summarise(n_total = sum(n),.groups = "drop"), by = "team.name") %>%
    mutate(percentage = n/n_total)


  borders_y <- seq(0, pitchWidth, pitchWidth/4)
  n_borders <- length(borders_y)

  borders_coords <- data.frame(x_start = pitchLength*2/3, y_start = borders_y[1:(n_borders-1)],
                               x_end = pitchLength*2/3,
                               y_end =  borders_y[2:n_borders],
                               zone = 1:4)

  df_counts %>% dplyr::left_join(borders_coords, by = "zone") %>% dplyr::mutate(zone = as.factor(zone))

}



#' Visualise attacking zones
#'
#' @param dfAttack data frame with information on zones and attacks as returned by
#' `compute_attacking_zones`
#' @param team Team's name for which to visualise attacking zones
#' @param pitchLength Length of pitch in data set
#' @param pitchWidth Width of pitch in data set
#' @param leftToRight Logical; whether the team is attacking from left to right
#' @param scale_lim limits on colour scale for arrow colour
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' data(eventData)
#' dfatt <- compute_attacking_zones(eventData)
#' plot_attack_zones(dfatt, team = "Borussia Dortmund")
#' plot_attack_zones(dfatt, team = "Bayern Munich", leftToRight = FALSE)
plot_attack_zones <- function(dfAttack, team, pitchLength = 120, pitchWidth = 80, leftToRight = TRUE,
                              scale_lim = c(0, 60)) {

  start_final_third <- pitchLength*2/3

  dfAttack <- dfAttack %>% filter(team.name == team)

  n_attacks <- sum(dfAttack$n, na.rm = TRUE)

  if(leftToRight) {
    reverse_scale <- scale_y_reverse
  } else {
    reverse_scale <- scale_x_reverse
  }

  step_size <- diff(seq(0, pitchWidth, pitchWidth/4))[1]

  arrow_y <- seq(step_size/2, pitchWidth, pitchWidth/4)

  arrow_coords <- data.frame(arrow_x_start = pitchLength/3,
                             arrow_y_start  = arrow_y, arrow_x_end = pitchLength*3/4,
                             arrow_y_end = arrow_y,
                             zone = factor(1:4))


  attackplot <- dfAttack %>% left_join(arrow_coords, by = "zone") %>%
    ggplot()+
    draw_pitch(dimensions = c(pitchLength, pitchWidth), palette = "bw", reverse_scale = reverse_scale) +
    reverse_scale()+
    geom_segment(aes(x = arrow_x_start, y = arrow_y_start, xend = arrow_x_end, yend = arrow_y_end,
                     colour = percentage*100),
                 arrow = arrow(),
                 lineend = "round", linejoin = "bevel", lwd = 8) +
    geom_label(aes( x = pitchLength/2, y = arrow_y_start, label = paste(round(percentage*100), "%"),
                    colour = round(percentage*100)), size = 8)+
    scale_colour_gradient(low = "#407DB7", high = "#B32047", limits = scale_lim) +
    geom_segment(aes( x = x_start, y = y_start, xend = x_end, yend = y_end))+
    geom_segment(aes( x = x_start, y = y_end, xend = pitchLength, yend = y_end)) +
    labs(colour = "Attacks (%)", title = paste("Attacks of", team, "by zone"),
         subtitle = paste(n_attacks, "attacks in total"))+
    theme_bw()+
    theme(axis.title = element_blank(), legend.position = "none", axis.text = element_blank())

  attackplot

}
