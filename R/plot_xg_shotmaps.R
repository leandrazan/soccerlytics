
#' Visualise shots with xG/outcome
#'
#' @param df Event data set
#' @param hometeam Name of hometeam (plays from left to right)
#' @param pitch_dim dimension of the pitch
#'
#' @return a list of ggplot objects
#' @export
#'
#' @examples
#' data(eventData)
#' shotmaps <- plot_shotmaps(eventData)
#' shotmaps$shot_types
#' shotmaps$shot_xg
plot_shotmaps <- function(df, hometeam = NULL, pitch_dim = c(120, 80)) {

  teams <- unique(df$team.name)
  if(is.null(hometeam)) {hometeam <- teams[1]}
  awayteam <- teams[!(teams == hometeam)]

  df$team.name <- factor(df$team.name, levels = c(hometeam, awayteam))
  ##### shots
  shot_stats <- df %>% filter(type.name == "Shot") %>%
    select(team.name, player.name, location.x, location.y, shot.statsbomb_xg, shot.outcome.name, minute, second) %>%
    unique()

  shot_stats <- shot_stats %>% mutate(location.x = ifelse(team.name == hometeam, location.x, pitch_dim[1] - location.x),
                        location.y = ifelse(team.name == hometeam, pitch_dim[2] - location.y, location.y))

  shots <- shot_stats %>%
    group_by(team.name, shot.outcome.name) %>%
    summarise( n = n(), .groups = "drop")

  shots_total <- shots %>% mutate(onTarget = ifelse(shot.outcome.name %in% c("Blocked", "Goal", "Saved"), TRUE, FALSE)) %>%
    group_by(team.name, onTarget) %>%
    summarise( n = sum(n), .groups = "drop") %>%
    pivot_wider(names_from = onTarget, names_prefix = "onTarget", values_from = n) %>%
    mutate(total = onTargetFALSE + onTargetTRUE)

  shots_xg <- shot_stats %>%  group_by(team.name) %>% summarise(xG = sum(shot.statsbomb_xg))

  shots_total <- shots_total %>% left_join(shots_xg, by = "team.name")

  shots_total <- shots_total %>% left_join(shots %>% spread(key = shot.outcome.name, value = n),by = "team.name")


  shotmap <- shot_stats %>%
    mutate(isGoal = ifelse(shot.outcome.name == "Goal", TRUE, FALSE)) %>%
    ggplot() +
    draw_pitch(dimensions = pitch_dim, palette = "bw") +
    geom_point(aes(x = location.x, y= location.y, colour = isGoal, shape = shot.outcome.name), size = 6)+
    geom_text(data = shot_stats %>% filter(shot.outcome.name == "Goal"),
              aes( x= location.x, y = location.y, label = paste("xG:", round(shot.statsbomb_xg, 2))), size = 5)+
    labs(title = paste("Shots"), colour = "Goal:", shape = "Outcome:")+
    facet_wrap(~team.name, scales = "free")+
    theme_bw() + theme(axis.text = element_blank(), text = element_text(size = 20), axis.title = element_blank(),
                       legend.position = "bottom")


  shotmap_xg <- shot_stats %>%
    mutate(isGoal = ifelse(shot.outcome.name == "Goal", TRUE, FALSE)) %>%
    ggplot() +
    draw_pitch(dimensions = pitch_dim, palette = "bw") +
    geom_point(aes(x = location.x, y= location.y, fill = shot.statsbomb_xg, colour = isGoal),
               shape = 21, size = 10, stroke = 2)+
    scale_fill_gradient(low = "red", high = "green")+
    scale_colour_manual(breaks = c(FALSE, TRUE), values = c("gray50", "blue"))+
    # coord_cartesian(xlim = c(70, 120))+
    labs(title = paste("Shots"), colour =  "Goal:", fill = "xG:")+
    guides(colour = guide_legend(order = 2))+
    geom_text(data = shot_stats %>%  filter(shot.outcome.name == "Goal"),
              aes( x = location.x, y = location.y, label = paste0(minute, "'")))+
    theme_bw() +  theme(axis.text = element_blank(), text = element_text(size = 20),
                        axis.title = element_blank(), legend.position = "bottom") +
    facet_wrap(~team.name, scales = "free")

  list(shot_types = shotmap, shot_xg = shotmap_xg)
}

